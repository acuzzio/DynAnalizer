module DataTypes where

import IntCoor

data Dynam = Dynam {
           gtRootN       :: Int,
           gtDT          :: Double,
           gtAType       :: String,
           gtStep        :: [Step]
           } deriving Show

data Step = Step {
        gtWf             :: [String],
        gtCoor           :: [Vec Double],
        gtCharge         :: [[Double]],
        gtDipole         :: [[Double]],
        gtEnePop         :: [Double],
        gtGrad           :: [Vec Double],
        gtVelo           :: [Vec Double],
        gtKin            :: Double,
        gtTot            :: Double 
        } deriving Show

data Dinamica = Dinamica {
          getOutputNam   :: String,
          getAtomN       :: Int,
          getRootN       :: Int,
          getStartRlxRt  :: Int,
          getDT          :: Double,
          getAtomT       :: [String],
          getEnergies    :: [[Double]],
          getCoordinates :: [Vec Double],
          getOscStr      :: [Double],
          getCharTran    :: [Double]
          } deriving Show

data PlotType = Pop | Ene | Dyn deriving (Eq,Show)

--data PlotType2 = Red | Black | Hop deriving (Eq,Show)

data Plottable =  BlaPlot
               | Root 
               | Jump 
               | InternalPlot [Int]
               | ChargePlot [Int]
               | EnergyPop
               | Empty
               deriving (Eq, Show, Read)

data Root = S0 | S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Show, Read, Enum)

data Flag = Help
            | CreateInfo     String
            | CreateInfo2    String
            | CreateInfoBin  String
            | CreateInfoQMMM String
            | CheckInfo      String
            | InputFile      String
            | Label          String
            deriving (Show, Eq)

data IsomType = Cis | Trans deriving (Show, Read)

type AllTrajData = [SingleTrajData]
type SingleTrajData = [PlottableData]
type PlottableData = [String]

data FileType = Binary | Normal deriving Show

