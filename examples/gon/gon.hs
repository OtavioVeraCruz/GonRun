
module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

data GameAttribute = GA Int Bool (GLdouble,GLdouble)
data ObjectAttribute = NoObjectAttribute 
data GameState = LevelStart Int | Level Int | GameOver
data TileAttribute = NoTileAttribute

type GONAction a = IOGame GameAttribute ObjectAttribute GameState TileAttribute a
type GONObject = GameObject ObjectAttribute
type GONTile = Tile TileAttribute
type GONMap = TileMatrix TileAttribute


--Tamanho do quadro e velocidade
tileSize, speedMod :: GLdouble
tileSize = 30.0
speedMod = 30.0

--Posição Inicial de Gon
initPos = (40.0,550.0) :: (GLdouble,GLdouble)


--OBJETIVO
objetivo :: Bool
objetivo = False

defaultTimer :: Int
defaultTimer = 20

white,black :: InvList
white = Just[(255,255,255)]
black = Just[(0,0,0)]
--mensagens
--personagem
--inimigos
--cenário
bmpList :: FilePictureList
bmpList = [
           ("stage1.bmp", Nothing),
           ("stage2.bmp", Nothing),
           ("stage3.bmp", Nothing),             
           ("stage4.bmp", Nothing),
           ("stage5.bmp", Nothing),
           ("gameover.bmp", Nothing),
           ("campeao.bmp", Nothing),
           ("gon.bmp", Nothing),
  	       ("hisoka.bmp",Nothing),
           ("mestre.bmp",Nothing),
           ("bordaHTop.bmp", Nothing),
           ("bordaHDown.bmp",   Nothing),
           ("bordaVLeft.bmp",   Nothing),
           ("bordaVRight.bmp",  Nothing),
           ("grass.bmp",           Nothing),
           ("hole.bmp",           Nothing),
           ("lama.bmp",Nothing),
           ("hunters.bmp",Nothing),
           ("portal.bmp",Nothing)]
           
bu,bd,bl,br,grass,hole,lama,hunters,portal :: Int
bu = 10
bd = 11
bl = 12
br = 13
grass  = 14
hole  = 15
lama = 16
hunters = 17
portal = 18


main :: IO ()
main = do
  let winConfig = ((200,100),(1050,600),"Gon Run")

      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize),
                          (tileMap map4 tileSize tileSize),
                          (tileMap map5 tileSize tileSize)] 0

      gameAttribute = GA defaultTimer objetivo initPos

      groups = [(objectGroup "messages"  createMsgs ),
                (objectGroup "gon"     [createGon]),
                (objectGroup "inimigos"     createInimigos)]
      input = [
               (SpecialKey KeyLeft,  Press, turnLeft ),
               (SpecialKey KeyRight, Press, turnRight),
               (SpecialKey KeyUp,    Press, turnUp   ),
               (SpecialKey KeyDown,  Press, turnDown )
              ,(Char 'q',            Press, \_ _ -> funExit)
              ]
  
  bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("examples/gon/"++a); return (a', b)}) bmpList
  funInit winConfig gameMap groups (LevelStart 1) gameAttribute input gameCycle (Timer 150) bmpList'

createMsgs :: [GONObject]
createMsgs =
  let stage1 = Tex(654,500) 0
      stage2 = Tex(654,500) 1 
      stage3 = Tex(654,500) 2
      stage4 = Tex(654,500) 3
      stage5 = Tex(654,500) 4
      gameOver = Tex (400,200) 5
      campeao = Tex (718,519) 6     
           		          	
  in [(object "level1"          stage1          True (555,300) (0,0) NoObjectAttribute),
      (object "level2"          stage2          True (555,300) (0,0) NoObjectAttribute),
      (object "level3"          stage3          True (555,300) (0,0) NoObjectAttribute),
      (object "level4"          stage4          True (555,300) (0,0) NoObjectAttribute),
      (object "level5"          stage5          True (555,300) (0,0) NoObjectAttribute),
      (object "gameover"        gameOver        True (555,300) (0,0) NoObjectAttribute),
      (object "campeao" campeao True (555,300) (0,0) NoObjectAttribute)
      ]	

createGon :: GONObject
createGon = let pic = Tex (tileSize*0.7,tileSize*0.7) 5
             in object "gon" pic True initPos (0,speedMod) NoObjectAttribute

createInimigos :: [GONObject]
createInimigos = let hisoka = Tex (tileSize*1.5,tileSize*1.3) 8
                     mestre = Tex (tileSize*3.0,tileSize*3.0) 9
           in[(object "hisoka1" hisoka True (45.0,405.0) (40,0) NoObjectAttribute),
              (object "hisoka2" hisoka True (45.0,205.0) (40,0) NoObjectAttribute),
              (object "hisoka3" hisoka True (45.0,105.0) (40,0) NoObjectAttribute),
	      (object "hisoka4" hisoka True (305.0,555.0) (40,0) NoObjectAttribute),
              (object "hisoka5" hisoka True (45.0,305.0) (40,0) NoObjectAttribute),
	      (object "hisoka6" hisoka True (205.0,305.0) (0,40) NoObjectAttribute),
	      (object "hisoka7" hisoka True (405.0,540.0) (0,40) NoObjectAttribute),
              (object "hisoka7" hisoka True (405.0,540.0) (0,40) NoObjectAttribute),
              (object "mestre" mestre True (205.0,300.0) (60,60) NoObjectAttribute)] 

moveInimigoH:: GONObject -> GONAction()
moveInimigoH inimigo = do 
	      col1 <- objectLeftMapCollision inimigo
              col2 <- objectRightMapCollision inimigo
              when (col1 || col2) (reverseXSpeed inimigo)
     
moveInimigoV :: GONObject -> GONAction()
moveInimigoV inimigo = do
	col1 <- objectTopMapCollision inimigo
        col2 <- objectBottomMapCollision inimigo
	when (col1 || col2) (reverseYSpeed inimigo)  	

moveInimigoEspecial :: GONObject -> GONAction()
moveInimigoEspecial inimigo = do
            moveInimigoV inimigo
            moveInimigoH inimigo
 
turnUp :: Modifiers -> Position -> GONAction ()
turnUp _ _ = do
  gon <- findObject "gon" "gon"
  gonPos <- getObjectPosition gon
  tile <- getTileFromWindowPosition gonPos
  if ((getTilePictureIndex tile) == 10)
	then do stop
	else do setObjectSpeed (0,speedMod) gon

turnDown :: Modifiers -> Position -> GONAction ()
turnDown _ _ = do
  gon <- findObject "gon" "gon"
  gonPos <- getObjectPosition gon
  tile <- getTileFromWindowPosition gonPos
  if ((getTilePictureIndex tile) == 11)
	then do stop
	else do setObjectSpeed (0,-speedMod) gon

turnLeft :: Modifiers -> Position -> GONAction ()
turnLeft _ _ = do
  gon <- findObject "gon" "gon"
  gonPos <- getObjectPosition gon
  tile <- getTileFromWindowPosition gonPos
  if ((getTilePictureIndex tile) == 12)
	then do stop
	else do setObjectSpeed (-speedMod,0) gon

    
turnRight :: Modifiers -> Position -> GONAction ()
turnRight _ _ = do
  gon <- findObject "gon" "gon"
  gonPos <- getObjectPosition gon
  tile <- getTileFromWindowPosition gonPos
  if ((getTilePictureIndex tile) == 13)
	then do stop
	else do setObjectSpeed (speedMod,0) gon

stop :: GONAction ()
stop = do
  gon <- findObject "gon" "gon"
  setObjectSpeed (0,0) gon


gameCycle :: GONAction ()
gameCycle = do
  (GA timer objetivo previousgonPos) <- getGameAttribute
  gState <- getGameState
  case gState of
      LevelStart n -> case n of
                        6 -> do
                              campeao <- findObject "campeao" "messages"
                              drawObject campeao
                              if (timer == 0)
                                  then funExit
                                  else (setGameAttribute (GA (timer - 1) objetivo previousgonPos))
                        n -> do
                              disableGameFlags
                              level <- findObject ("level" ++ (show n)) "messages"
                              drawObject level
                              if (timer == 0)
                                  then (do setGameState (Level n)
                                           enableGameFlags
                                           gon <- findObject "gon" "gon"
                                           setObjectAsleep False gon
                                           setObjectPosition initPos gon
                                           setObjectSpeed (0.0,0.0) gon
                                           setObjectCurrentPicture 7 gon 
                                           setGameAttribute (GA defaultTimer objetivo previousgonPos)
                                           destroyObject level
                                           setNewMap n)
                                  else setGameAttribute (GA (timer - 1) objetivo previousgonPos)
			  
      Level n -> do
                  gon <- findObject "gon" "gon"
                  if (objetivo==True) -- advance level!
                      then  (do setGameState (LevelStart (n + 1))
                                disableGameFlags
                                setGameAttribute (GA timer False initPos)                                   
				)
                                else (do checkCollision gon
                                         when (n==1) (do
                                           hisokaEnemy1 <- findObject "hisoka1" "inimigos"
                                           moveInimigoH hisokaEnemy1  
                                           hisokaEnemy2 <- findObject "hisoka2" "inimigos" 
                                           moveInimigoH hisokaEnemy2  
                                      	   hisokaEnemy3 <- findObject "hisoka3" "inimigos" 
                                           moveInimigoH hisokaEnemy3
                                           setObjectAsleep False hisokaEnemy1    
                                           setObjectAsleep False hisokaEnemy2    
				           setObjectAsleep False hisokaEnemy3
                                          )                                          
                                         when(n==2) (do
                                          hisokaEnemy1 <- findObject "hisoka1" "inimigos"
                                          moveInimigoH hisokaEnemy1
				          hisokaEnemy2 <- findObject "hisoka2" "inimigos" 
                                          moveInimigoH hisokaEnemy2  
                                      	  hisokaEnemy3 <- findObject "hisoka3" "inimigos" 
                                          moveInimigoH hisokaEnemy3 
					  hisokaEnemy4 <- findObject "hisoka4" "inimigos"	
                                          moveInimigoH hisokaEnemy4
                                          hisokaEnemy6 <- findObject "hisoka6" "inimigos" 
                                          moveInimigoV hisokaEnemy6 
                                          setObjectAsleep False hisokaEnemy1    
                                          setObjectAsleep False hisokaEnemy2    
				          setObjectAsleep False hisokaEnemy3
                                          setObjectAsleep False hisokaEnemy4
                                          setObjectAsleep False hisokaEnemy6       		                                  
                                          )
                                         when(n==3)(do
					  hisokaEnemy1 <- findObject "hisoka1" "inimigos"
                                          moveInimigoH hisokaEnemy1
				          hisokaEnemy2 <- findObject "hisoka2" "inimigos" 
                                          moveInimigoH hisokaEnemy2  
                                      	  hisokaEnemy3 <- findObject "hisoka3" "inimigos" 
                                          moveInimigoH hisokaEnemy3
                                          hisokaEnemy4 <- findObject "hisoka4" "inimigos" 
                                          moveInimigoH hisokaEnemy4
                                          hisokaEnemy5 <- findObject "hisoka5" "inimigos" 
                                          moveInimigoH hisokaEnemy5
                                          hisokaEnemy7 <- findObject "hisoka7" "inimigos" 
                                          moveInimigoV hisokaEnemy7
                                          --troque para o 7                                     
                                          setObjectAsleep False hisokaEnemy1    
                                          setObjectAsleep False hisokaEnemy2    
				          setObjectAsleep False hisokaEnemy3
                                          setObjectAsleep False hisokaEnemy4    
				          setObjectAsleep False hisokaEnemy5
                                          setObjectAsleep False hisokaEnemy7    
				        
					  )
					 when (n==4) (do
                                          hisokaEnemy1 <- findObject "hisoka1" "inimigos"
                                          moveInimigoH hisokaEnemy1
				          hisokaEnemy2 <- findObject "hisoka2" "inimigos" 
                                          moveInimigoH hisokaEnemy2  
                                      	  hisokaEnemy3 <- findObject "hisoka3" "inimigos" 
                                          moveInimigoH hisokaEnemy3
                                          hisokaEnemy4 <- findObject "hisoka4" "inimigos" 
                                          moveInimigoH hisokaEnemy4
                                          hisokaEnemy5 <- findObject "hisoka5" "inimigos" 
                                          moveInimigoH hisokaEnemy5
                                          hisokaEnemy6 <- findObject "hisoka6" "inimigos" 
                                          moveInimigoV hisokaEnemy6
                                          hisokaEnemy7 <- findObject "hisoka7" "inimigos" 
                                          moveInimigoV hisokaEnemy7                        
                                          setObjectAsleep False hisokaEnemy1    
                                          setObjectAsleep False hisokaEnemy2    
				          setObjectAsleep False hisokaEnemy3
                                          setObjectAsleep False hisokaEnemy4    
				          setObjectAsleep False hisokaEnemy5
                                          setObjectAsleep False hisokaEnemy6 
                                          setObjectAsleep False hisokaEnemy7     
                                          ) 
				 	 when (n==5) (do
					  hisokaEnemy1 <- findObject "hisoka1" "inimigos"
                                          moveInimigoH hisokaEnemy1
				          hisokaEnemy2 <- findObject "hisoka2" "inimigos" 
                                          moveInimigoH hisokaEnemy2  
                                      	  hisokaEnemy3 <- findObject "hisoka3" "inimigos" 
                                          moveInimigoH hisokaEnemy3
                                          hisokaEnemy4 <- findObject "hisoka4" "inimigos" 
                                          moveInimigoH hisokaEnemy4
                                          hisokaEnemy5 <- findObject "hisoka5" "inimigos" 
                                          moveInimigoH hisokaEnemy5
                                          hisokaEnemy6 <- findObject "hisoka6" "inimigos" 
                                          moveInimigoV hisokaEnemy6
                                          hisokaEnemy7 <- findObject "hisoka7" "inimigos" 
                                          moveInimigoV hisokaEnemy7 
                                          mestreEnemy <- findObject "mestre" "inimigos"
                                          moveInimigoEspecial mestreEnemy                                        
                                          setObjectAsleep False hisokaEnemy1    
                                          setObjectAsleep False hisokaEnemy2    
				          setObjectAsleep False hisokaEnemy3
                                          setObjectAsleep False hisokaEnemy4    
				          setObjectAsleep False hisokaEnemy5
                                          setObjectAsleep False hisokaEnemy6    
				          setObjectAsleep False hisokaEnemy7  
                                          setObjectAsleep False mestreEnemy 
                                          ) 
						
					 gonPos <- getObjectPosition gon
 					 tile <- getTileFromWindowPosition gonPos
 					 if((getTilePictureIndex tile)==18) 
                                           then setGameAttribute(GA timer True initPos)
				     	   else return ())
                 
      GameOver -> do
                      disableMapDrawing
                      gameover <- findObject "gameover" "messages"
                      drawMap
                      drawObject gameover
                      if (timer == 0)
                              then funExit
                              else (setGameAttribute (GA (timer - 1) objetivo (0,0)))


setNewMap :: Int -> GONAction ()
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap 4 = setCurrentMapIndex 3
setNewMap 5 = setCurrentMapIndex 4
setNewMap _ = return ()

checkCollision :: GONObject -> GONAction ()
checkCollision gon = do
  gonPos <- getObjectPosition gon
  tile <- getTileFromWindowPosition gonPos
  inimigos <- getObjectsFromGroup "inimigos"
  col <- objectListObjectCollision inimigos gon
  v <- getObjectSpeed gon
  
  when ((getTilePictureIndex tile)==16) (setObjectSpeed ((fst v)*0.7,(snd v)*0.7) gon)
  --when ((getTilePictureIndex tile)==17) (setObjectSpeed (speedMod-15,0) gon) 

  if ((getTileBlocked tile || col ))
          then (do setGameState GameOver
                   disableObjectsDrawing
                   disableObjectsMoving
                   setGameAttribute (GA defaultTimer objetivo (0,0)))
          else if (((getTilePictureIndex tile)==10) || ((getTilePictureIndex tile) == 11))
          then reverseYSpeed gon
          else if( ((getTilePictureIndex tile) == 12) ||        ((getTilePictureIndex tile) == 13))
          then reverseXSpeed gon
          else return()


x,y,w,z,f,l,s,goal :: GONTile

x= (bl,False,0.0,NoTileAttribute)
y= (br,False,0.0,NoTileAttribute)
w= (bu,False,0.0,NoTileAttribute)
z= (bd,False,0.0,NoTileAttribute)
f = (grass,   False, 0.0, NoTileAttribute)
l = (lama,False,0.0,NoTileAttribute)
s = (hole,True,0.0,NoTileAttribute)
goal = (portal,False,0.0,NoTileAttribute)


map1 :: GONMap
map1 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,s,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,s,s,s,s,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,s,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,f,f,f,f,f,f,f,y],
        [s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,goal,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]



map2 :: GONMap
map2 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,s,f,f,f,f,goal,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]

map3 :: GONMap
map3 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,s,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,goal,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]

map4 :: GONMap
map4 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,goal,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]


map5 :: GONMap	
map5 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,s,f,goal,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,s,s,s,s,s,s],
        [x,f,f,f,f,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,s,s,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]	


