/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Madanagopsl
 *
 */
public class CalculationElementComponent
{


  private static final String SUB_CALCULATION_UNIT_Title = "Sub Calculation Unit";

  class ListLabelProvider extends LabelProvider
  {
    public Image getImage( Object element )
    {
      return null;
    }

    public String getText( Object element )
    {
      if( element instanceof IFeatureWrapper2 )
      {

        String name = ((IFeatureWrapper2) element).getName();
        if( name != null )
        {
          return name;
        }
        else
        {
          return ((IFeatureWrapper2) element).getGmlID();
        }
      }
      else
      {
        throw new RuntimeException( "Only IFeatureWrapper2 is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element );
      }
    }
  }

  
  /* ======================================================================== */
  private TableViewer tableViewer;
  private Image imageBoundaryUp;
  final String mainGroupTitle = "Bitte Höhenmodell auswählen";
  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";
  final String deleteSelected = "Geländemodell löschen";
  final String defaultTestDecription = "Wählen Sie ein Modell aus.";
  final String saveToolTip = "Deskription Sichern";
  final String titleDescriptionGroup = "Beschreibung";
  private Group descriptionGroupText;
  private Text descriptionText;
  private Text _2dElementField;
  private Text _1dElementField;
  private GridData data;
  private boolean boundaryUPState = false;
  private boolean boundaryDownState = false;
  private Image imageBoundaryDown;
  private FormToolkit toolkit;
  private Composite parent;
  private KeyBasedDataModel dataModel;

  public void createControl( KeyBasedDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiSelectFromList( parent );
  }
  
  private void guiSelectFromList( Composite parent )
  {
    Composite rootComposite = new Composite (parent,SWT.FLAT);
    rootComposite.setLayout( new GridLayout(1,false) );
    
    Composite optionsComposite = new Composite(rootComposite,SWT.FLAT);
    optionsComposite.setLayout( new GridLayout(2,false));
    
    Label _1dElementLabel = new Label(optionsComposite, SWT.RIGHT);
    _1dElementLabel.setText("1D Element: ");
    
    _1dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    _1dElementField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    _1dElementField.setLayoutData(data);
    
    Label _2dELementLabel = new Label(optionsComposite, SWT.RIGHT);
    _2dELementLabel.setText("2D Element: ");
    
    _2dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    _2dElementField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    _2dElementField.setLayoutData(data);
    
    Label BoundaryUpLabel = new Label(optionsComposite, SWT.RIGHT);
    BoundaryUpLabel.setText("Boundary Up: ");
    
    Button BoundaryUpBtn = new Button(optionsComposite,SWT.CENTER|SWT.PUSH);
    
    if (boundaryUPState)
    {
      imageBoundaryUp = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
            "icons/elcl16/inputOk.gif" ).getImageData() );
    }
    else
    {
      imageBoundaryUp = new Image( optionsComposite.getDisplay(), 
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/inputError.gif" ).getImageData() );      
    }
    
    BoundaryUpBtn.setImage( imageBoundaryUp );
    
    Label BoundaryDownLabel = new Label(optionsComposite, SWT.RIGHT);
    BoundaryDownLabel.setText("Boundary Down: ");
    
    Button BoundaryDownBtn = new Button(optionsComposite,SWT.CENTER|SWT.PUSH);
    
    if (boundaryDownState )
    {
      imageBoundaryDown = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
            "icons/elcl16/inputOk.gif" ).getImageData() );
    }
    else
    {
      imageBoundaryDown = new Image( optionsComposite.getDisplay(), 
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/inputError.gif" ).getImageData() );      
    }
    
    BoundaryDownBtn.setImage( imageBoundaryDown );   
    
    Composite subCalculationComposite = new Composite(rootComposite,SWT.FLAT);
    subCalculationComposite.setLayout( new FormLayout() );
    
    Label titleSubCalculation = new Label(subCalculationComposite,SWT.NONE);
    titleSubCalculation.setText(SUB_CALCULATION_UNIT_Title);
    FormData formData = new FormData();
    formData.top = new FormAttachment(optionsComposite,5);
    titleSubCalculation.setLayoutData( formData );
    
    
    tableViewer = new TableViewer( subCalculationComposite, SWT.FILL | SWT.BORDER );
    Table table = tableViewer.getTable();
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( new ListLabelProvider() );
    table.setLinesVisible( true );
    
    formData = new FormData();
    formData.top = new FormAttachment(titleSubCalculation,5);
    formData.left = new FormAttachment(0,5);
    formData.bottom = new FormAttachment(100,0);
    table.setLayoutData( formData );
    
    descriptionGroupText = new Group( subCalculationComposite, SWT.NONE );
    descriptionGroupText.setText( titleDescriptionGroup );
    formData = new FormData();
    formData.left = new FormAttachment( table, 5 );
    formData.top = new FormAttachment( titleSubCalculation, 10 );
    formData.right = new FormAttachment(100,0);
    formData.bottom = new FormAttachment( 100, 0 );
    descriptionGroupText.setLayoutData( formData );

    FormLayout formDescription = new FormLayout();
    descriptionGroupText.setLayout( formDescription );
    
    descriptionText = new Text( descriptionGroupText, SWT.MULTI|SWT.WRAP );
    descriptionText.setText( defaultTestDecription );
    descriptionText.setEditable( false );
    
    FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.right = new FormAttachment( 100, 0 );
    descriptionText.setLayoutData(formDescripData);    
  }
}
