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
 */
public class CalculationUnitPerformWidgetFace
{
  private final CalculationUnitDataModel m_dataModel;

  public CalculationUnitPerformWidgetFace( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Composite createControl( final Composite parent, final FormToolkit toolkit )
  {
    final ScrolledForm form = toolkit.createScrolledForm( parent );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );
    // form.setText(Messages.getString("CalculationUnitPerformWidgetFace.0")); //$NON-NLS-1$
    form.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section
    final Section selectCalcUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    selectCalcUnitSection.setText( Messages.getString( "CalculationUnitPerformWidgetFace.1" ) ); //$NON-NLS-1$
    final TableWrapData tableWrapDataCU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // tableWrapDataCU.grabHorizontal = true;
    // tableWrapDataCU.grabVertical = true;
    selectCalcUnitSection.setLayoutData( tableWrapDataCU );
    selectCalcUnitSection.setExpanded( true );

    // Creates Section for "Calculation Elements Unit"
    final Section calculationElementUnitSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationElementUnitSection.setText( Messages.getString( "CalculationUnitPerformWidgetFace.2" ) ); //$NON-NLS-1$
    final TableWrapData tableWrapDataCE = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // tableWrapDataCE.grabHorizontal = true;
    // tableWrapDataCE.grabVertical = true;
    calculationElementUnitSection.setLayoutData( tableWrapDataCE );
    calculationElementUnitSection.setExpanded( true );

    // Creates Section for "Calculation Settings Unit"
    final Section problemsSection = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    problemsSection.setText( Messages.getString( "CalculationUnitPerformWidgetFace.3" ) ); //$NON-NLS-1$
    final TableWrapData tableWrapDataPU = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    // tableWrapDataPU.grabHorizontal = true;
    // tableWrapDataPU.grabVertical = true;
    problemsSection.setLayoutData( tableWrapDataPU );
    problemsSection.setExpanded( true );

    final Composite sectionFirstComposite = createCalculationUnitSection( selectCalcUnitSection, toolkit );
    createCalculationElementsSection( calculationElementUnitSection, toolkit );
    createProblemsInCalculationSection( problemsSection, sectionFirstComposite, toolkit );
    return parent;
  }

  private Composite createCalculationUnitSection( final Section selectCalcUnitSection, final FormToolkit toolkit )
  {
    selectCalcUnitSection.setLayout( new FillLayout() );
    final Composite sectionFirstComposite = toolkit.createComposite( selectCalcUnitSection, SWT.FLAT );
    selectCalcUnitSection.setClient( sectionFirstComposite );
    final FormLayout formLayout = new FormLayout();
    sectionFirstComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    sectionFirstComposite.setLayoutData( formData );

    final CalculationUnitPerformComponent calcSelect = new CalculationUnitPerformComponent( m_dataModel );
    calcSelect.createControl( sectionFirstComposite );

    return sectionFirstComposite;
  }

  private void createCalculationElementsSection( final Section calculationElementUnitSection, final FormToolkit toolkit )
  {
    calculationElementUnitSection.setLayout( new FillLayout() );
    final Composite sectionThirdComposite = toolkit.createComposite( calculationElementUnitSection, SWT.FLAT );
    calculationElementUnitSection.setClient( sectionThirdComposite );
    final FormLayout formLayout = new FormLayout();
    sectionThirdComposite.setLayout( formLayout );

    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionThirdComposite, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    sectionThirdComposite.setLayoutData( formData );

    final SelectedCalculationComponent calcElementGUI = new SelectedCalculationComponent( m_dataModel );
    calcElementGUI.createControl( toolkit, sectionThirdComposite );
  }

  private void createProblemsInCalculationSection( final Section problemsSection, final Composite sectionFirstComposite, final FormToolkit toolkit )
  {
    problemsSection.setLayout( new FillLayout() );

    final Composite sectionSecondComposite = toolkit.createComposite( problemsSection, SWT.FLAT );
    problemsSection.setClient( sectionSecondComposite );

    final FormLayout formLayout = new FormLayout();
    sectionSecondComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionFirstComposite, 5 );
    sectionSecondComposite.setLayoutData( formData );

    final CalculationUnitLogComponent calcProblemsGUI = new CalculationUnitLogComponent( m_dataModel );
    calcProblemsGUI.createControl( toolkit, sectionSecondComposite );
  }
}
