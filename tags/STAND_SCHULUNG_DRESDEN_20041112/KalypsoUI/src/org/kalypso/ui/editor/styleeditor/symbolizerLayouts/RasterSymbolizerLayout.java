package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Symbolizer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 *  
 */

public class RasterSymbolizerLayout extends AbstractSymbolizerLayout
{

  public RasterSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer,
      KalypsoUserStyle m_userStyle )
  {
    super( m_composite, m_symbolizer, m_userStyle );
  }

  public void draw()
  {
    GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    // ***** Label Group
    Group labelGroup = new Group( composite, SWT.NULL );
    GridData labelGroupData = new GridData();
    labelGroupData.widthHint = 210;
    labelGroupData.heightHint = 246;
    labelGroup.setLayoutData( labelGroupData );
    labelGroup.setLayout( compositeLayout );
    labelGroup.layout();

    Label label = new Label( labelGroup, SWT.NULL );
    label.setText( MessageBundle.STYLE_EDITOR_ERROR_NO_IMPLEMENTATION );
  }
}