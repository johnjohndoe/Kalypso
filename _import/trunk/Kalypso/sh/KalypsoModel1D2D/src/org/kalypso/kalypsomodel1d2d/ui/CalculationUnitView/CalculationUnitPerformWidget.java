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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitPerformWidget implements IWidgetWithOptions, 
                                                     IWidget, 
                                                     IWidgetWithStrategy,
                                                     IGrabDistanceProvider
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

  private String name;

  private String toolTip;
  /**
   * The constructor.
   */
  
  public CalculationUnitPerformWidget() {
    this("Calculation Unit Perform","Berechnungseinheiten Modellieren");
  }

  public CalculationUnitPerformWidget(String name, String toolTip ){
    this.name = name;
    this.toolTip = toolTip;
  }

  public void createPartControl( Composite parent )
  {
    m_parent = parent;
    toolkit = new FormToolkit(parent.getDisplay());
    form = toolkit.createScrolledForm(parent);
    form.setText("Calculation Unit Perform"); 
    form.getBody().setLayout(new TableWrapLayout());

    // Calculation Unit Section     
    selectCalcUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    selectCalcUnitSection.setText( "Berechnungseinheiten" );
    final TableWrapData tableWrapDataCU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapDataCU.grabHorizontal = true;
    tableWrapDataCU.grabVertical = true;
    selectCalcUnitSection.setLayoutData( tableWrapDataCU );
    selectCalcUnitSection.setExpanded(true);

    // Creates Section for "Calculation Settings Unit"
    problemsSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    problemsSection.setText( "Berechnungseinheit Verwalten" );
    final TableWrapData tableWrapDataPU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapDataPU.grabHorizontal = true;
    tableWrapDataPU.grabVertical = true;
    problemsSection.setLayoutData( tableWrapDataPU );
    problemsSection.setExpanded(true);    
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
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    // TODO Auto-generated method stub
    return null;
  }


  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    // TODO Auto-generated method stub
    return false;
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    return null;
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    // TODO Auto-generated method stub
    return null;
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy#setStrategy(org.kalypso.ogc.gml.widgets.IWidget)
   */
  public void setStrategy( IWidget strategy )
  {
    // TODO Auto-generated method stub
    
  }


  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider#getGrabDistance()
   */
  public double getGrabDistance( )
  {
    // TODO Auto-generated method stub
    return 0;
  }

}
