package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Symbolizer;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 *
 */

public class RasterSymbolizerLayout extends SymbolizerLayout{ 
  
  public RasterSymbolizerLayout(Composite composite, Symbolizer symbolizer, KalypsoUserStyle userStyle)
  {
    super(composite,symbolizer,userStyle);
  }
  
  public void draw() throws FilterEvaluationException
  {     
    GridLayout compositeLayout = new GridLayout();    
    compositeLayout.marginHeight = 2; 
        
    // ***** Label Group
    Group labelGroup = new Group(composite,SWT.NULL);   
    GridData labelGroupData = new GridData();
    labelGroupData.widthHint = 210;
    labelGroupData.heightHint = 298;  
    labelGroup.setLayoutData(labelGroupData);     
    labelGroup.setLayout(compositeLayout);
    labelGroup.layout();
    
    Label label = new Label(labelGroup,SWT.NULL);
    label.setText("Not implemented yet");
  } 
}
