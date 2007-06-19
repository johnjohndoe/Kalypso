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
package org.kalypso.kalypsomodel1d2d.ui.view.calculation_unit;

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
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitAdministerComponent;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitComponent;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitPerformView extends ViewPart
{
  private FormToolkit toolkit;
  private ScrolledForm form;
  private Composite rootPanel;
  private Section selectCalcUnitSection;
  private Section problemsSection;
  private Section calculationElementUnitSection;
  private Composite sectionFirstComposite;
  private CalculationUnitPerformComponent calcSelect;
  private KeyBasedDataModel dataModel;
  private Composite sectionSecondComposite;
  private CalculationUnitProblemsComponent calcProblemsGUI;
  /**
   * The constructor.
   */
  
  
  public CalculationUnitPerformView() {
  }

  public CalculationUnitPerformView(KeyBasedDataModel dataModel) {
    this.dataModel = dataModel;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    toolkit = new FormToolkit(parent.getDisplay());
    form = toolkit.createScrolledForm(parent);
    form.setText("Calculation Unit Perform"); 
    TableWrapData tableWrapData;

    form.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section     
    selectCalcUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    selectCalcUnitSection.setText( "Berechnungseinheiten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    selectCalcUnitSection.setLayoutData( tableWrapData );
    selectCalcUnitSection.setExpanded( true );

 // Creates Section for "Calculation Settings Unit"
    problemsSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    problemsSection.setText( "Berechnungseinheit Verwalten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    problemsSection.setLayoutData( tableWrapData );
    problemsSection.setExpanded( true );
 
   
    createCalculationUnitSection( selectCalcUnitSection );
    createProblemsInCalculationSection(problemsSection);
    
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
