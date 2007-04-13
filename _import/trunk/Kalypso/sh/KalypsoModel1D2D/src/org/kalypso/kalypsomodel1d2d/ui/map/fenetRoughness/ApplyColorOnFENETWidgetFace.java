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
package org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * @author Madanagopal
 *
 */
public class ApplyColorOnFENETWidgetFace
{

//  public Control createControl( Composite parent )
//  {
//    // TODO Auto-generated method stub
//    return null;
//  }
//
//  public void disposeControl( )
//  {
//    // TODO Auto-generated method stub
//    
//  }
  static int index = 0;
  private Composite rootPanel;
  private FormToolkit toolkit;

  private ListViewer areaViewer;

  private Section colorAndButtonSection;

  private ColorOnFEDataModel dataModel;


  public ApplyColorOnFENETWidgetFace( ColorOnFEDataModel feDataModel )
  {
   this.dataModel = feDataModel;
  }
  private IFeatureSelectionListener featureSelectionListener = new IFeatureSelectionListener()
  {

    public void selectionChanged( IFeatureSelection selection )
    {
      // TODO Auto-generated method stub
      
    }
    //TODO
  };
  private IRoughnessPolygonCollection roughnessPolygons;
  private IFEDiscretisationModel1d2d nodeModel;
  
  public Control createControl( Composite parent )
  {
    this.dataModel.getMapPanel().getSelectionManager().addSelectionListener( featureSelectionListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Creates Section for "Select Elevation Model"
    colorAndButtonSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    colorAndButtonSection.setText( "Apply Colors" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    colorAndButtonSection.setLayoutData( tableWrapData );
    colorAndButtonSection.setExpanded( true );

    createSelectElevationModel( colorAndButtonSection );
    return rootPanel;
  }

   private final void createSelectElevationModel( Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    // clientComposite.setSize( 400, 300 );
    FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 5 );
    clientComposite.setLayoutData( formData );
       
    Button activateColorBtn = new Button(clientComposite,SWT.PUSH);
    formData = new FormData();
    formData.top = new FormAttachment(0,5);
    formData.left = new FormAttachment(0,5);
    activateColorBtn.setText( "Activate Colors" );
    activateColorBtn.setLayoutData( formData );
    activateColorBtn.addSelectionListener( new SelectionAdapter(){
      public void widgetSelected( SelectionEvent e )
      {
        paintRoughnessColors();        
      }      
    });
    
    Composite newComposite = new Composite(clientComposite,SWT.None);
    formData = new FormData();
    formData.left = new FormAttachment(0,5);
    formData.top = new FormAttachment(activateColorBtn,5);
    formData.bottom = new FormAttachment(100, -5);
    newComposite.setLayoutData( formData );
    newComposite.setLayout( new GridLayout(1,false) );
    
    final ColorFieldEditor maxColorSelector = new ColorFieldEditor( "All Color", "Select Farbe", newComposite);
    Button buttonMax = maxColorSelector.getColorSelector().getButton();
    buttonMax.setLayoutData( new GridData( GridData.CENTER, GridData.CENTER, false, false ) );
    
  }


  protected void paintRoughnessColors( )
  {
    nodeModel = dataModel.getDiscretisationModel();
    roughnessPolygons = dataModel.getRoughnessPolygonCollection();
    for (IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> inte: nodeModel.getElements()) {
       try
      {
         //roughnessPolygons.getSelectedPolygons( point )
       IRoughnessEstimateSpec roughnessEstimateSpec = roughnessPolygons.getRoughnessEstimateSpec( inte.recalculateElementGeometry());
       //roughnessEstimateSpec.getContributingRoughnessPolygons()mostSpreadRoughness(); 
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }    
    }
    
//  roughnessPolygons.getRoughnessEstimateSpec( inte. );
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
    MapPanel mapPanel = dataModel.getMapPanel();
    if( mapPanel != null )
    {
      mapPanel.getSelectionManager().addSelectionListener( featureSelectionListener );
    }
    
  }
  
  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";
 
  private void initStoreDefaults( )
  {

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

  // TODO patrice use scheduling rule to wait for map load
  // See OpenMapViewCommand
  private static final void waitOnMap( IMapModell mapModell, MapPanel mapPanel )
  {
  }

}
