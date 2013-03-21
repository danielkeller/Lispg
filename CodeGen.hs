{-# LANGUAGE ScopedTypeVariables #-}

module CodeGen (
    cgTest,
    codeGen
) where

import General
import Builtins

import LLVM.Core
import qualified LLVM.Core as L(Value)
import Data.TypeLevel hiding (Bool, Eq)
import Data.Int
import Data.Word
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List(intercalate)

--intermediate binder type for compiling to IR
data LLBinder = ContB | COffs Int | ArgB | ExternB String
    | ClosB (Map.Map LLBinder LLBinder) --this is kind of a hack to store the creation of a closure
    deriving (Eq, Ord)

instance Binder LLBinder where
    printBdr (COffs n) = "#" ++ show n
    printBdr ArgB = "arg"
    printBdr ContB = "k"
    printBdr (ExternB n) = n
    printBdr (ClosB b) = "(" ++ conts ++ ")"
        where conts = intercalate ", " $ map printMap $ Map.toList b
              printMap (l, r) = printBdr l ++ " -> " ++ printBdr r
    contVar = ContB

closureBind :: [String] -> GInline String -> GInline LLBinder
closureBind globals ex = cbInline ex $ Map.empty
    where cbInline (Inline ex) m = Inline $ cbExpr ex m
          cbInline f@(Fun a cps) m =
              let clos = Set.toList $ freeIn f `Set.intersection` Map.keysSet m
                      -- ^everything actually being closed over
                  m' = Map.fromList $ zip clos (map COffs [0..]) --new String->LLBinder mapping
                  reass = ClosB $ Map.mapKeys (\k -> m Map.! k) m' --closure reassignments
              in Fun reass $ cbCps cps (Map.insert a ArgB m')
          cbCps (Call f a c) m = Call (cbInline f m) (cbInline a m) (cbCont c m)
          cbCps (CCC v) m = CCC $ cbInline v m
          cbCps (Case alts) m = Case $ map (cbAlt m) alts
          cbAlt m (CAlt i c) = CAlt (cbInline i m) (cbCps c m)
          cbCont (Cont a cps) m = let Fun a' cps' = cbInline (Fun a cps) m
                                  in Cont a' cps' --same semantics, different name
          cbExpr (EVar v) m = EVar $ case Map.lookup v m of
                  Just b -> b
                  Nothing -> ExternB v --probably not needed
          cbExpr (EApp f a) m = EApp (cbExpr f m) (cbExpr a m)
          cbExpr (ELit l) _ = ELit l
          --cbExpr bad _ = error $ "unexpected " ++ printExpr bad ++ " in cps code"

type BlockList = Map.Map String BasicBlock
data GlobalInfo = GlobalInfo {blocks :: BlockList,
                              cur_cont :: L.Value (Ptr Label),
                              cc_env :: L.Value (Ptr (Array D10 (Ptr Word8))),
                              cur_arg :: L.Value (Ptr Word8)}

codeGen :: String -> CFile -> IO ()
codeGen fName file = do
    putStrLn $ intercalate "\n\n" $ flip map file $ \(CBinding b e) ->
        let topLevels = Map.keys builtins ++ map (\(CBinding b e) -> b) file
        in b ++ " = " ++ (printCps . CCC . closureBind topLevels) e
    mod <- newModule
    defineModule mod $ do
        let f_main argc argv = do
            a_cur_cont :: L.Value (Ptr Label) <- alloca
            a_cc_env :: L.Value (Ptr (Array D10 (Ptr Word8))) <- alloca
            a_cur_arg :: L.Value (Ptr Word8) <- alloca
            blockList <- (flip mapM) file $ \ (CBinding n _ ) -> do
                bb <- newNamedBasicBlock n
                return (n, bb)
            let global = GlobalInfo (Map.fromList blockList) a_cur_cont a_cc_env a_cur_arg
            mapM (cgBind global) file
            ret (0 :: Int32) 
        createNamedFunction ExternalLinkage "main" f_main :: TFunction(Int32 -> Ptr (Ptr Word8) -> IO Int32)
        return ()
    writeBitcodeToFile fName mod 

cgBind :: GlobalInfo -> CBinding -> CodeGenFunction r ()
cgBind g (CBinding name (Fun arg body)) = do
    defineBasicBlock $ (blocks g) Map.! name
    return ()

cgCps :: GlobalInfo -> Cps -> CodeGenFunction r ()
cgCps g (CCC inl) = return ()

cgTest = do
    mod <- newModule
    defineModule mod $ do
        f_printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Int32)
        let f_main argc argv = do
            withStringNul "Hello, World!\n" $ \hwString -> do
                tmp <- getElementPtr0 hwString (0 :: Word32, ())
                call (castVarArgs f_printf :: Function (Ptr Word8 -> IO Int32)) tmp
            ret (0 :: Int32) 
        createNamedFunction ExternalLinkage "main" f_main :: TFunction(Int32 -> Ptr (Ptr Word8) -> IO Int32)
        return ()
    writeBitcodeToFile "test.bc" mod 

