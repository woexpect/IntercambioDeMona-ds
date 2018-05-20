{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Laminas where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..), renderBootstrap3, bfs)
import Yesod.Form.Nic (nicHtmlField)

laminaForm :: Form Lamina
laminaForm = renderBootstrap3 BootstrapBasicForm $ Lamina
            <$> areq textField (bfs MsgNewLaminaNumber) Nothing
            <*> areq textField (bfs MsgNewLaminaTitle) Nothing
            <*> areq textField (bfs MsgNewLaminaSeccion) Nothing
            <*> areq nicHtmlField (bfs MsgNewLaminasContent) Nothing

monaForm :: LaminaId -> Form Mona
monaForm laminaId = renderBootstrap3 BootstrapBasicForm $ Mona
            <$> pure laminaId
            <*> lift (liftIO getCurrentTime)
            <*> lift requireAuthId
            <*> areq textField (bfs MsgOffertTitle) Nothing
            <*> areq textareaField (bfs MsgInformationtext) Nothing
-----------------------------------------------------------------
-----------------------------------------------------------------
getLaminaR :: Handler Html
getLaminaR = do
    laminas <- runDB $ selectList [] [Desc LaminaNumero]
    (laminaWidget, enctype) <- generateFormPost laminaForm
    defaultLayout $ do 
        setTitleI MsgLaminas
        $(widgetFile "laminas/index")

getMonaR :: LaminaId -> Handler Html
getMonaR laminaId = do
    (lamina, monas) <- runDB $ do
        lamina <- get404 laminaId
        monas <- selectList [MonaLaminaId ==. laminaId] [Asc MonaPosted]
        return (lamina, monas)
    (monaWidget, enctype) <- generateFormPost (monaForm laminaId)
    defaultLayout $ do
        setTitleI $ MsgLaminaTitle $ laminaNumero lamina
        $(widgetFile "laminas/monas")