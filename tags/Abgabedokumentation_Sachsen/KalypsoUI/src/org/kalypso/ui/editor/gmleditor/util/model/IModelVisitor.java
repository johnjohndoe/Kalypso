package org.kalypso.ui.editor.gmleditor.util.model;

/**
 * Zum Rekursiven durchlaufen eines Model-Trees
 * 
 * @author belger
 */
public interface IModelVisitor
{
  /**
   * Besucht dieses Element
   * 
   * @return false, falls die Rekursion an dieser Stelle abgebrochen werden kann
   */
  public boolean visit( final Model model );
}