package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Symbolizer;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author F.Lindemann
 *  
 */
public abstract class AbstractSymbolizerLayout
{

  protected Composite composite = null;

  protected Symbolizer symbolizer = null;

  protected KalypsoUserStyle userStyle = null;

  protected AbstractSymbolizerLayout( Composite m_parent, Symbolizer m_symbolizer,
      KalypsoUserStyle m_userStyle )
  {
    this.composite = m_parent;
    this.symbolizer = m_symbolizer;
    this.userStyle = m_userStyle;
  }

  protected AbstractSymbolizerLayout( Composite parent )
  {
    this.composite = parent;
  }

  public abstract void draw() throws FilterEvaluationException;
}