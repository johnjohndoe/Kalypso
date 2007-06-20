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
package org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.ogc.gml.map.MapPanel;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitPerformView extends ViewPart
{
  public static final String ID = "org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.CalculationUnitPerformView";
  
  private FormToolkit toolkit;
  private ScrolledForm form;
  private Composite rootPanel;
  private Section selectCalcUnitSection;
  private Section problemsSection;
  private Section calculationElementUnitSection;
  private Composite sectionFirstComposite;
  private CalculationUnitPerformComponent calcSelect;
  private CalculationUnitDataModel dataModel = new CalculationUnitDataModel();
  private Composite sectionSecondComposite;
  private CalculationUnitProblemsComponent calcProblemsGUI;
  private MapPanel mapPanel;

  private IFEDiscretisationModel1d2d m_model;

  private Composite m_parent;
  /**
   * The constructor.
   */
  
  public CalculationUnitPerformView() {
  }


  public void createPartControl( Composite parent )
  {
    m_parent = parent;
//    initialiseModel( null );
    toolkit = new FormToolkit(parent.getDisplay());
    form = toolkit.createScrolledForm(parent);
    form.setText("Calculation Unit Perform"); 
    form.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section     
    selectCalcUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    selectCalcUnitSection.setText( "Berechnungseinheiten" );
    final TableWrapData tableWrapDataCU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapDataCU.grabHorizontal = true;
    tableWrapDataCU.grabVertical = true;
    selectCalcUnitSection.setLayoutData( tableWrapDataCU );
    selectCalcUnitSection.setExpanded( true );

 // Creates Section for "Calculation Settings Unit"
    problemsSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    problemsSection.setText( "Berechnungseinheit Verwalten" );
    final TableWrapData tableWrapDataPU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapDataPU.grabHorizontal = true;
    tableWrapDataPU.grabVertical = true;
    problemsSection.setLayoutData( tableWrapDataPU );
    problemsSection.setExpanded( true );
    
//    createProblemsInCalculationSection(problemsSection);
    
    /*
    createCalculationUnitSection( selectCalcUnitSection );
    createProblemsInCalculationSection(problemsSection);
    m_parent.update();
    m_parent.pack();
    m_parent.redraw();
    */
  }


  public void initialiseModel( IFEDiscretisationModel1d2d model )
  {
    m_model = model;
    
//    IFEDiscretisationModel1d2d m_model1 = Util.getModel( IFEDiscretisationModel1d2d.class );
//    dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    //TODO check model1d2d for null and do something
    dataModel.setData( 
        ICommonKeys.KEY_DISCRETISATION_MODEL, m_model );
    dataModel.setData(
        ICommonKeys.KEY_FEATURE_WRAPPER_LIST, 
        CalUnitOps.getModelCalculationUnits( m_model ) );
    dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );
    
    //command manager since it is use in the dirty pool object framework
    //the commandable workspace of the target theme is taken
    dataModel.setData( ICommonKeys.KEY_COMMAND_MANAGER, m_model.getWrappedFeature().getWorkspace());
    
    dataModel.setData( 
        ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, 
        this );
    
    createCalculationUnitSection( selectCalcUnitSection );
    createProblemsInCalculationSection(problemsSection);
    m_parent.update();
    m_parent.pack();
    m_parent.redraw();
    
  }

  private void createCalculationUnitSection( Section selectCalcUnitSection )
  {
    selectCalcUnitSection.setLayout( new FillLayout() );
    sectionFirstComposite = toolkit.createComposite( selectCalcUnitSection, SWT.FLAT );
    selectCalcUnitSection.setClient( sectionFirstComposite );
    FormLayout formLayout = new FormLayout();
    sectionFirstComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    sectionFirstComposite.setLayoutData( formData );
    
    calcSelect = new CalculationUnitPerformComponent();    
    calcSelect.createControl( dataModel, toolkit, sectionFirstComposite );
    
  }

  private void createProblemsInCalculationSection( Section problemsSection )
  {
    problemsSection.setLayout( new FillLayout() );
    sectionSecondComposite = toolkit.createComposite( problemsSection, SWT.FLAT );
    problemsSection.setClient( sectionSecondComposite );
    
    FormLayout formLayout = new FormLayout();
    sectionSecondComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionFirstComposite, 5 );
    //formData.bottom = new FormAttachment( sectionThirdComposite, -5 );
    sectionSecondComposite.setLayoutData( formData );
    
    calcProblemsGUI = new CalculationUnitProblemsComponent();
    calcProblemsGUI.createControl( dataModel, toolkit, sectionSecondComposite ); 
    
    
  }
  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {

  }

}
