package org.kalypso.ui.editor.gmleditor.util.model.visitors;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypso.ui.editor.gmleditor.util.model.IModelVisitor;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;

/**
 * Findet alle FeatureElemente, die Features aus einer gegebenen Liste haben
 * 
 * @author belger
 */
public class FindDataElementsVisitor implements IModelVisitor
{
  /** Zum schnellen suchen */
  final Collection m_objecthash = new HashSet();

  final Collection m_elements = new ArrayList();

  public FindDataElementsVisitor( final Object[] fats )
  {
    m_objecthash.addAll( Arrays.asList( fats ) );
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IModelVisitor#visit(org.kalypso.ui.editor.gmleditor.util.model.Model)
   */
  public boolean visit( final Model model )
  {
    if( model instanceof PropertyElement && m_objecthash.contains( ( (PropertyElement)model ).getProperty() ) )
      m_elements.add( model );
    else if( model instanceof FeatureElement && m_objecthash.contains( ( (FeatureElement)model ).getFeature() ) )
      m_elements.add( model );

    return true;
  }

  public IModel[] getResults()
  {
    return (IModel[])m_elements.toArray( new IModel[m_elements.size()] );
  }
}
