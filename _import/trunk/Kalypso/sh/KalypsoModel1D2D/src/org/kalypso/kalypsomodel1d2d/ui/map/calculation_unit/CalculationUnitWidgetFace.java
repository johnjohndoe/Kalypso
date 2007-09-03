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

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 */
class CalculationUnitWidgetFace
{

  static int index = 0;

  private Composite m_rootPanel;

  private FormToolkit m_toolkit;

  private Section calculationUnitSection;

  private CalculationUnitComponent calcGUI;

  private CalculationUnitDataModel m_dataModel;

  private Section calculationSettingsSection;

  private Composite sectionFirstComposite;

  private Composite sectionThirdComposite;

  private SelectedCalculationComponent calcElementGUI;

  private Section calculationElementUnitSection;

  private Composite sectionSecondComposite;

  private CalculationUnitAdministerComponent calcComplexSelectionGUI;

  public CalculationUnitWidgetFace( )
  {

  }

  public CalculationUnitWidgetFace( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final Composite parent )
  {
    parent.setLayout( new FillLayout() );
    m_rootPanel = new Composite( parent, SWT.FILL );
    m_rootPanel.setLayout( new FillLayout() );
    m_toolkit = new FormToolkit( parent.getDisplay() );
    final ScrolledForm scrolledForm = m_toolkit.createScrolledForm( m_rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section
    calculationUnitSection = m_toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationUnitSection.setText( "Berechnungseinheiten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationUnitSection.setLayoutData( tableWrapData );
    calculationUnitSection.setExpanded( true );

    // Creates Section for "Calculation Settings Unit"
    calculationSettingsSection = m_toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationSettingsSection.setText( "Berechnungseinheit Verwalten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationSettingsSection.setLayoutData( tableWrapData );
    calculationSettingsSection.setExpanded( true );

    // Creates Section for "Calculation Elements Unit"
    calculationElementUnitSection = m_toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationElementUnitSection.setText( "Status der selektierten Berechnungseinheit" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationElementUnitSection.setLayoutData( tableWrapData );
    calculationElementUnitSection.setExpanded( true );

    createCalculationUnit( calculationUnitSection );
    createCalculationSettingsSection( calculationSettingsSection );
    createCalculationElements( calculationElementUnitSection );

    return m_rootPanel;
  }

  private final void createCalculationUnit( final Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );
    sectionFirstComposite = m_toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( sectionFirstComposite );
    final FormLayout formLayout = new FormLayout();
    sectionFirstComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    sectionFirstComposite.setLayoutData( formData );
    calcGUI = new CalculationUnitComponent();
    calcGUI.createControl( m_dataModel, m_toolkit, sectionFirstComposite );
  }

  private void createCalculationSettingsSection( final Section settingsSection )
  {
    settingsSection.setLayout( new FillLayout() );
    sectionSecondComposite = m_toolkit.createComposite( settingsSection, SWT.FLAT );
    settingsSection.setClient( sectionSecondComposite );

    final FormLayout formLayout = new FormLayout();
    sectionSecondComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionFirstComposite, 5 );
    // formData.bottom = new FormAttachment( sectionThirdComposite, -5 );
    sectionSecondComposite.setLayoutData( formData );

    calcComplexSelectionGUI = new CalculationUnitAdministerComponent( m_dataModel );
    calcComplexSelectionGUI.createControl( sectionSecondComposite );
  }

  private void createCalculationElements( final Section elementStatusSection )
  {

    elementStatusSection.setLayout( new FillLayout() );
    sectionThirdComposite = m_toolkit.createComposite( elementStatusSection, SWT.FLAT );
    elementStatusSection.setClient( sectionThirdComposite );
    final FormLayout formLayout = new FormLayout();
    sectionThirdComposite.setLayout( formLayout );

    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionThirdComposite, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    sectionThirdComposite.setLayoutData( formData );

    calcElementGUI = new SelectedCalculationComponent();
    calcElementGUI.createControl( m_dataModel, m_toolkit, sectionThirdComposite );
  }

  public void disposeControl( )
  {
    if( m_rootPanel == null )
      return;
    if( !m_rootPanel.isDisposed() )
    {
      m_rootPanel.dispose();
      m_toolkit.dispose();
    }
  }
}