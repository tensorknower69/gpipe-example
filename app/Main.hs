{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Word
import Graphics.GPipe
import System.Random
import qualified Graphics.GPipe.Context.GLFW as GLFW

getDualKeyState :: (MonadIO m, Num a) => Window os c ds -> GLFW.Key -> GLFW.Key -> ContextT GLFW.Handle os m (Maybe a)
getDualKeyState window minusKey plusKey =
	runMaybeT $ do
		minus <- isKeyPressed minusKey
		plus <- isKeyPressed plusKey
		pure $ fromIntegral $ fromEnum plus - fromEnum minus
	where
		isKeyPressed key = do
			keyState <- MaybeT $ GLFW.getKey window key
			pure $ keyState == GLFW.KeyState'Pressed

getDualKeyDirectionState
	:: (MonadIO m, Num a)
	=> Window os c ds
	-> (GLFW.Key, GLFW.Key)
	-> (GLFW.Key, GLFW.Key)
	-> (GLFW.Key, GLFW.Key)
	-> ContextT GLFW.Handle os m (Maybe (V3 a))
getDualKeyDirectionState window xDualKeys yDualKeys zDualKeys =
	runMaybeT $ do
		xDualState <- go xDualKeys
		yDualState <- go yDualKeys
		zDualState <- go zDualKeys
		pure $ V3 xDualState yDualState zDualState
	where
		go dualKeys = MaybeT $ uncurry (getDualKeyState window) dualKeys


v2ToRatio :: (Integral a, Fractional b) => V2 a -> b
v2ToRatio (V2 x y) = (fromIntegral x) / (fromIntegral y)

makeTranslationMatrix :: Num a => V3 a -> M44 a
makeTranslationMatrix = mkTransformationMat identity

makeRotationMatrix :: Num a => Quaternion a -> M44 a
makeRotationMatrix = m33_to_m44 . fromQuaternion

toEulerXYZ :: RealFloat a => Quaternion a -> V3 a
toEulerXYZ (Quaternion w (V3 x y z)) = V3 pitch yaw roll
	where
		sinp = 2 * (w * y - z * x)
		pitch = if abs sinp >= 1 then (signum sinp) * pi / 2 else asin sinp

		siny_cosp = 2 * (w * z + x * y)
		cosy_cosp = 1 - 2 * (y * y + z * z)
		yaw = atan2 siny_cosp cosy_cosp

		sinr_cosp = 2 * (w * x + y * z)
		cosr_cosp = 1 - 2 * (x * x + y * y)
		roll = atan2 sinr_cosp cosr_cosp

fromEulerXYZ :: Floating a => V3 a -> Quaternion a
fromEulerXYZ (V3 pitch yaw roll) = Quaternion w (V3 x y z)
	where
		halfSinCos angle = let angle' = angle / 2 in (sin angle', cos angle')
		(sp, cp) = halfSinCos pitch
		(sy, cy) = halfSinCos yaw
		(sr, cr) = halfSinCos roll

		w = cp * cy * cr + sp * sy * sr
		x = sp * cy * cr - cp * sy * sr
		y = cp * sy * cr + sp * cy * sr
		z = cp * cy * sr - sp * sy * cr

data Camera a
	= Camera
		{ _cameraPosition :: V3 a
		, _cameraQuaternion :: Quaternion a
		, _cameraProjectionMatrix :: M44 a
		}
	deriving (Show, Eq)

makeLenses ''Camera

makeCameraViewMatrix :: (Conjugate a, RealFloat a) => Camera a -> M44 a
makeCameraViewMatrix camera = rotation !*! translation
	where
		translation = makeTranslationMatrix $ negate $ camera ^. cameraPosition
		rotation = makeRotationMatrix $ conjugate $ camera ^. cameraQuaternion

runLoop window vertexBuffer uniformBuffer shader camera texture2D lastTime = do
	Just currentTime <- liftIO $ GLFW.getTime
	let dt = currentTime - lastTime

	Just (movementDirectionState :: V3 Double) <- getDualKeyDirectionState
		window
		(GLFW.Key'A, GLFW.Key'D)
		(GLFW.Key'LeftShift, GLFW.Key'Space)
		(GLFW.Key'W, GLFW.Key'S)
	Just (rotationDirectionState :: V3 Double) <- getDualKeyDirectionState
		window
		(GLFW.Key'Down, GLFW.Key'Up)
		(GLFW.Key'Right, GLFW.Key'Left)
		(GLFW.Key'E, GLFW.Key'Q)

	frameBufferSize <- getFrameBufferSize window
	
	let
		movementSpeed = 4
		rotationSpeed = 2
		aspectRatio = v2ToRatio frameBufferSize
		cameraQuaternionDelta = fromEulerXYZ (rotationDirectionState * pure (rotationSpeed * dt))
		cameraQuaternion' = camera ^. cameraQuaternion * cameraQuaternionDelta
		cameraPositionDelta = rotate cameraQuaternion' movementDirectionState * pure (movementSpeed * dt)
		cameraPosition' = camera ^. cameraPosition + cameraPositionDelta
		camera' = camera
			& cameraProjectionMatrix .~ perspective (pi/3) aspectRatio 0.001 1000.0
			& cameraQuaternion .~ cameraQuaternion'
			& cameraPosition .~ cameraPosition'

	pixelsLazy <- liftIO $ replicateM 64 (randomIO :: IO Word32)
	writeTexture2D texture2D 0 (V2 0 0) (V2 8 8) pixelsLazy

	writeBuffer uniformBuffer 0
		[ (fmap realToFrac <$> camera' ^. cameraProjectionMatrix, fmap realToFrac <$> makeCameraViewMatrix camera')
		]
	
	render $ do
		clearWindowColor window (V3 0.05 0.05 0.05)
		vertexArray <- newVertexArray vertexBuffer
		let primitiveArray = toPrimitiveArray TriangleList vertexArray
		shader $
			( primitiveArray
			, (FrontAndBack, ViewPort (V2 0 0) frameBufferSize, DepthRange 0 1)
			)

	swapWindowBuffers window
	closeRequested <- GLFW.windowShouldClose window
	unless (closeRequested == Just True) $ do
		runLoop window vertexBuffer uniformBuffer shader camera' texture2D currentTime

main :: IO ()
main = do
	runContextT GLFW.defaultHandleConfig $ do
		window <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "test")
		vertexBuffer :: Buffer os (B3 Float, B2 Float) <- newBuffer 6
		writeBuffer vertexBuffer 0
			[ (V3 (-0.5)   0.5   0.0, V2 0 0)
			, (V3 (-0.5) (-0.5)  0.0, V2 0 1)
			, (V3   0.5    0.5   0.0, V2 1 0)
			, (V3   0.5    0.5   0.0, V2 1 0)
			, (V3 (-0.5) (-0.5)  0.0, V2 0 1)
			, (V3   0.5  (-0.5)  0.0, V2 1 1)
			]
		uniformBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float))) <- newBuffer 1
		writeBuffer uniformBuffer 0
			[ (identity, identity)
			]
		texture2D <- newTexture2D R8 (V2 8 8) 1
		let
			whiteBlack = cycle [minBound,maxBound] :: [Word32]
			blackWhite = tail whiteBlack
		writeTexture2D texture2D 0 (V2 0 0) (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))
		shader <- compileShader $ do
			(projectionMatrix, viewMatrix) <- getUniform (const (uniformBuffer, 0))
			primitiveStream <- toPrimitiveStream (view _1)

			let
				primitiveStream' = primitiveStream
					& fmap (\(V3 x y z, uv) -> (projectionMatrix !* (viewMatrix !* V4 x y z 1), uv))
			fragmentStream <- rasterize (view _2) primitiveStream'

			let
				filter = SamplerFilter Nearest Nearest Nearest Nothing
				edge = (pure Repeat, undefined)
			sampler2D <- newSampler2D (const (texture2D, filter, edge))

			let
				sampleTexture = pure . sample2D sampler2D SampleAuto Nothing Nothing
				fragmentStream' = fragmentStream
					& fmap sampleTexture

			drawWindowColor (const (window, ContextColorOption NoBlending (V3 True True True))) fragmentStream'
		let
			camera = Camera
				{ _cameraPosition = V3 0 0 2
				, _cameraQuaternion = Quaternion 1 (V3 0 0 0)
				, _cameraProjectionMatrix = identity
				}
		runLoop window vertexBuffer uniformBuffer shader camera texture2D 0
