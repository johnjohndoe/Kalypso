package org.kalypso.ui.editor.gmleditor.util.model;

public interface IModelVisitor
{
  public void visitMovingBox( FeatureElement box, Object passAlongArgument );

  public void visitBook( FeatureElement book, Object passAlongArgument );

  public void visitBoardgame( FeatureElement boardgame, Object passAlongArgument );
}