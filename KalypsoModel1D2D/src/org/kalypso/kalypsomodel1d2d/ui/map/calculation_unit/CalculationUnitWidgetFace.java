/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
class CalculationUnitWidgetFace
{
  private final CalculationUnitDataModel m_dataModel;

  public CalculationUnitWidgetFace( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final Form form = toolkit.createForm( parent );

    final Composite body = form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    // Calculation Unit Section
    final Section calculationUnitSection = toolkit.createSection( body, Section.EXPANDED | Section.TITLE_BAR );
    calculationUnitSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidgetFace.0" ) ); //$NON-NLS-1$
    final GridData unitsSectionData = new GridData( SWT.FILL, SWT.FILL, true, true );
    calculationUnitSection.setLayoutData( unitsSectionData );

    // Creates Section for "Calculation Settings Unit"
    final Section calculationSettingsSection = toolkit.createSection( body, Section.EXPANDED | Section.TITLE_BAR );
    calculationSettingsSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidgetFace.1" ) ); //$NON-NLS-1$
    calculationSettingsSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    // Creates Section for "Calculation Elements Unit"
    final Section calculationElementUnitSection = toolkit.createSection( body, Section.EXPANDED | Section.TITLE_BAR );
    calculationElementUnitSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidgetFace.2" ) ); //$NON-NLS-1$
    calculationElementUnitSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    createCalculationUnit( calculationUnitSection, toolkit );
    createCalculationSettingsSection( calculationSettingsSection, toolkit );
    createCalculationElements( calculationElementUnitSection, toolkit );

//    body.layout( true, true );

    return body;
  }

  private final void createCalculationUnit( final Section workStatusSection, final FormToolkit toolkit )
  {
    final CalculationUnitMetaTable calcGUI = new CalculationUnitMetaTable( m_dataModel, CalculationUnitMetaTable.BTN_SHOW_AND_MAXIMIZE, CalculationUnitMetaTable.BTN_REMOVE, CalculationUnitMetaTable.BTN_ADD, CalculationUnitMetaTable.BTN_CLONE, CalculationUnitMetaTable.BTN_EDIT );
    final Control client = calcGUI.createControl( workStatusSection, toolkit );
    workStatusSection.setClient( client );
  }

  private void createCalculationSettingsSection( final Section settingsSection, final FormToolkit toolkit )
  {
    final Composite sectionSecondComposite = toolkit.createComposite( settingsSection, SWT.FLAT );
    settingsSection.setClient( sectionSecondComposite );

    sectionSecondComposite.setLayout( new FormLayout() );

    final CalculationUnitAdministerComponent calcComplexSelectionGUI = new CalculationUnitAdministerComponent( m_dataModel );
    calcComplexSelectionGUI.createControl( sectionSecondComposite );
  }

  private void createCalculationElements( final Section elementStatusSection, final FormToolkit toolkit )
  {
    final SelectedCalculationComponent calcElementGUI = new SelectedCalculationComponent( m_dataModel );
    final Control client = calcElementGUI.createControl( elementStatusSection, toolkit );
    elementStatusSection.setClient( client );
  }
}