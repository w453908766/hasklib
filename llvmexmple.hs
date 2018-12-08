
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Short.Internal as BS
import Data.Functor
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Monoid
import Foreign.Ptr
import Data.Word

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic
import LLVM.AST
import LLVM.AST.Type as A.T
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.RMWOperation as RMWOp

mAST = Module BS.empty BS.empty Nothing Nothing [
          GlobalDefinition $ globalVariableDefaults {
            G.name = Name BS.empty,
            G.type' = i32,
            G.isConstant = True,
            G.initializer = Just $ C.Int 32 42
          },
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = UnName 0,
            G.basicBlocks = [
              BasicBlock (UnName 1) [
                UnName 2 := GetElementPtr {
                  inBounds = True,
                  address = ConstantOperand (C.GlobalReference (ptr i32) (Name BS.empty)),
                  indices = [ ConstantOperand (C.Int 32 0) ],
                  metadata = []
                },
                UnName 3 := Load {
                  volatile = False,
                  address = LocalReference (ptr i32) (UnName 2),
                  maybeAtomicity = Nothing,
                  alignment = 1,
                  metadata = []
                }
              ] (
                Do $ Ret (Just (LocalReference i32 (UnName 3))) []
              )
             ]
           }
          ]
          
mStr = "; ModuleID = '<string>'\n\
               \source_filename = \"<string>\"\n\
               \\n\
               \@0 = constant i32 42\n\
               \\n\
               \define i32 @1() {\n\
               \  %1 = load i32, i32* @0, align 1\n\
               \  ret i32 %1\n\
               \}\n"

mass = "@0 = global i32 3\
              \define i32 @1(i32 %x) {\n\
              \  %1 = mul i32 %x, %x\n\
              \  %2 = add i32 %1, 3\n\
              \  ret i32 %2\n\
              \}\n"

s = withContext $ \context -> withModuleFromAST context mAST moduleLLVMAssembly              
t = withContext $ \context -> withModuleFromLLVMAssembly context mass moduleAST
main :: IO ()
main = do
  ss <- s
  tt <- t
  print ss
  print tt
