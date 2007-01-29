package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import java.util.Iterator;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.shapeImportWizards.utils.StyleUtils;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

public class PageStyleSelection extends WizardPage implements Listener {

  public PageStyleSelection() {
    this( Messages.getString("PageStyleSelection.0"), Messages.getString("PageStyleSelection.1") );//$NON-NLS-1$
    setDescription(Messages.getString("PageStyleSelection.2")); //$NON-NLS-1$
  }

  private Composite m_Composite;

  protected PageStyleSelection(String name, String title) {
    super(name, title, null);
  }

  public void handleEvent(Event arg0) {

    // TODO Auto-generated method stub

  }

  public void createColorControls() {
    DataContainer data = ((ImportWizard)getWizard()).m_data;
    StyleUtils utils = new StyleUtils(data.getOutputFile(), data.getOutputFile().substring( 0, data.getOutputFile().lastIndexOf( ".gml" )) + ".sld", null, data.getShapeProperty(), null, "Naziv lejera");
    try
    {
      data.setFilterPropertyColorMap( utils.getFilterpropertyColorMap() );
    }
    catch (Exception e)
    {
      return;
    }
//    if(data.getFilterPropertyColorMap() == null) return;
    // create the desired layout for this wizard page
    GridLayout gl = new GridLayout();
    int ncol = 3;
    gl.numColumns = ncol;

    final Fill markFill;
//  if( mark.getFill() == null )
    markFill = StyleFactory.createFill();
//  else
//  markFill = mark.getFill();

//  ColorChooserPanel[] colorPanel = new A();

    Iterator<String> strSetIterator = data.getFilterPropertyColorMap().keySet().iterator();
    while(strSetIterator.hasNext())
    {
      ColorChooserPanel fillColorChooserPanel = new ColorChooserPanel( m_Composite, "Some text 1", data.getFilterPropertyColorMap().get( strSetIterator.next() ) );
      createLine(m_Composite, ncol);
      fillColorChooserPanel.addColorChooserListener( 
          new PanelListener()
          {
            public void valueChanged( PanelEvent event )
            {
              org.eclipse.swt.graphics.Color color = ((ColorChooserPanel) event.getSource()).getColor();
              markFill.setFill( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
              //      userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
            }
          }
      );
    }
  }

  public void createControl(Composite parent) {
    Composite composite =  new Composite(parent, SWT.NULL);
    m_Composite = composite;
    setControl(composite);      
  }

  private void createLine(Composite parent, int ncol) 
  {
    Label line = new Label(parent, SWT.SEPARATOR|SWT.HORIZONTAL|SWT.BOLD);
    GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
    gridData.horizontalSpan = ncol;
    line.setLayoutData(gridData);
  }

}
