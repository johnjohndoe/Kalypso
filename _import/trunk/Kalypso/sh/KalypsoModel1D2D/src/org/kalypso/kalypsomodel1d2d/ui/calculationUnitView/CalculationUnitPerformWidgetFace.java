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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

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
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent;

/**
 * @author Madanagopal
 * 
 */
public class CalculationUnitPerformWidgetFace
{

  private Composite m_parent;

  private FormToolkit toolkit;

  private ScrolledForm form;

  private Section selectCalcUnitSection;

  // private Section problemsSection;
  private Composite sectionFirstComposite;

  private CalculationUnitPerformComponent calcSelect;

  private Composite sectionSecondComposite;

  // private CalculationUnitProblemsComponent calcProblemsGUI;
  private CalculationUnitDataModel dataModel;

  private Section calculationElementUnitSection;

  private Composite sectionThirdComposite;

  private SelectedCalculationComponent calcElementGUI;

  public CalculationUnitPerformWidgetFace( )
  {
  }

  public CalculationUnitPerformWidgetFace( CalculationUnitDataModel dataModel )
  {
    this.dataModel = dataModel;
  }

  public Composite createControl( Composite parent )
  {
    m_parent = parent;
    toolkit = new FormToolkit( parent.getDisplay() );
    form = toolkit.createScrolledForm( parent );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );
    // form.setText(Messages.getString("CalculationUnitPerformWidgetFace.0")); //$NON-NLS-1$
    form.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section
    selectCalcUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    selectCalcUnitSection.setText( Messages.getString( "CalculationUnitPerformWidgetFace.1" ) ); //$NON-NLS-1$
    final TableWrapData tableWrapDataCU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // tableWrapDataCU.grabHorizontal = true;
    // tableWrapDataCU.grabVertical = true;
    selectCalcUnitSection.setLayoutData( tableWrapDataCU );
    selectCalcUnitSection.setExpanded( true );

    // Creates Section for "Calculation Elements Unit"
    calculationElementUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationElementUnitSection.setText( Messages.getString( "CalculationUnitPerformWidgetFace.2" ) ); //$NON-NLS-1$
    TableWrapData tableWrapDataCE = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // tableWrapDataCE.grabHorizontal = true;
    // tableWrapDataCE.grabVertical = true;
    calculationElementUnitSection.setLayoutData( tableWrapDataCE );
    calculationElementUnitSection.setExpanded( true );

    // Creates Section for "Calculation Settings Unit"
    // problemsSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT |
    // Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    // problemsSection.setText( Messages.getString("CalculationUnitPerformWidgetFace.3") ); //$NON-NLS-1$
    // final TableWrapData tableWrapDataPU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // // tableWrapDataPU.grabHorizontal = true;
    // // tableWrapDataPU.grabVertical = true;
    // problemsSection.setLayoutData( tableWrapDataPU );
    // problemsSection.setExpanded(true);

    createCalculationUnitSection( selectCalcUnitSection );
    createCalculationElementsSection( calculationElementUnitSection );
    // createProblemsInCalculationSection(problemsSection);
    return parent;
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

    calcSelect = new CalculationUnitPerformComponent( dataModel );
    calcSelect.createControl( dataModel, sectionFirstComposite );

  }

  private void createCalculationElementsSection( Section calculationElementUnitSection )
  {
    calculationElementUnitSection.setLayout( new FillLayout() );
    sectionThirdComposite = toolkit.createComposite( calculationElementUnitSection, SWT.FLAT );
    calculationElementUnitSection.setClient( sectionThirdComposite );
    FormLayout formLayout = new FormLayout();
    sectionThirdComposite.setLayout( formLayout );

    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionThirdComposite, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    sectionThirdComposite.setLayoutData( formData );

    calcElementGUI = new SelectedCalculationComponent();
    calcElementGUI.createControl( dataModel, toolkit, sectionThirdComposite );

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
    sectionSecondComposite.setLayoutData( formData );

    // calcProblemsGUI = new CalculationUnitProblemsComponent();
    // calcProblemsGUI.createControl( dataModel, toolkit, sectionSecondComposite );
  }

  public void disposeControl( )
  {

    if( m_parent == null )
    {
      System.out.println( "Disposing null root panel" ); //$NON-NLS-1$
      return;
    }
    if( !m_parent.isDisposed() )
    {
      m_parent.dispose();
      toolkit.dispose();
    }
  }
}
