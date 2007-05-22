/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.List;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 */
class CalculationUnitWidgetFace
{
  static int index = 0;
  private Composite rootPanel;
  private FormToolkit toolkit;

  private ListViewer complexElementViewer;
  private Section elevationColorSection;

  private TableViewer elevationListTableViewer;
  private Section calculationUnitSection;
  private CalculationUnitComponent calcGUI;
  private CalculationUnitDataModel dataModel;
  private Section calculationElementUnitSection;
  private Composite sectionFirstComposite;
  private Composite sectionSecondComposite;
  private CalculationElementComponent calcElementGUI;
  public CalculationUnitWidgetFace( )
  {
    
  }
  public CalculationUnitWidgetFace( CalculationUnitDataModel dataModel )
  {
    this.dataModel = dataModel;
  }


  public Control createControl( Composite parent )
  {
    initStoreDefaults();
    
    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section     
    calculationUnitSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationUnitSection.setText( "Berechnungseinheiten Modellieren" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationUnitSection.setLayoutData( tableWrapData );
    calculationUnitSection.setExpanded( true );

    // Creates Section for "Calculation Elements Unit"
    calculationElementUnitSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationElementUnitSection.setText( "Calculation Elements" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationElementUnitSection.setLayoutData( tableWrapData );
    calculationElementUnitSection.setExpanded( true );
    
    createCalculationUnit( calculationUnitSection );
    createCalculationElements(calculationElementUnitSection);

    return rootPanel;
  }

  private final void createCalculationUnit( Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );
    sectionFirstComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( sectionFirstComposite );
    // clientComposite.setSize( 400, 300 );
    FormLayout formLayout = new FormLayout();
    sectionFirstComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    //formData.bottom = new FormAttachment( sectionSecondComposite, 5 );
    sectionFirstComposite.setLayoutData( formData );
    calcGUI= new CalculationUnitComponent();    
    calcGUI.createControl( dataModel, toolkit, sectionFirstComposite );
  }

  private void createCalculationElements( Section elementStatusSection )
  {
    
    elementStatusSection.setLayout( new FillLayout() );
    sectionSecondComposite = toolkit.createComposite( elementStatusSection, SWT.FLAT );
    elementStatusSection.setClient( sectionSecondComposite );
    FormLayout formLayout = new FormLayout();
    sectionSecondComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionSecondComposite, 5 );
    formData.bottom = new FormAttachment( 100, 5 );
    sectionSecondComposite.setLayoutData( formData );
    calcElementGUI= new CalculationElementComponent();    
    calcElementGUI.createControl( dataModel, toolkit, sectionSecondComposite );    
  }

  public void disposeControl( )
  {
  
    if( rootPanel == null )
    {
      System.out.println( "Disposing null root panel" );
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      rootPanel.dispose();
      toolkit.dispose();
    }
  
  }
  
  private IntegerFieldEditor handleWidth;

  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";

  private List selectionNodeList;

  private Table table;
  
  private TableViewer nodeElevationViewer;
  private void initStoreDefaults( )
  {

      //@TODO in case of using preferences 
  }
  
  private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {

      public void propertyChange( PropertyChangeEvent event )
      {
        Object source = event.getSource();
        String property = event.getProperty();

        if( source instanceof FieldEditor )
        {
          ((FieldEditor) source).store();
        }
        else if( source instanceof ColorSelector )
        {
        }
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() );
        }
      }

    };
  }

}