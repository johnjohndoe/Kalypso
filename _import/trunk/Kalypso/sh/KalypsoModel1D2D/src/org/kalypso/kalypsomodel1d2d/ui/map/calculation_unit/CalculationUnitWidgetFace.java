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

  private Composite rootPanel;

  private FormToolkit toolkit;

  private Section calculationUnitSection;

  private CalculationUnitComponent calcGUI;

  private CalculationUnitDataModel dataModel;

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

  public CalculationUnitWidgetFace( CalculationUnitDataModel dataModel )
  {
    this.dataModel = dataModel;
  }

  public Control createControl( Composite parent )
  {
    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Calculation Unit Section
    calculationUnitSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationUnitSection.setText( "Berechnungseinheiten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationUnitSection.setLayoutData( tableWrapData );
    calculationUnitSection.setExpanded( true );

    // Creates Section for "Calculation Settings Unit"
    calculationSettingsSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationSettingsSection.setText( "Berechnungseinheit Verwalten" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationSettingsSection.setLayoutData( tableWrapData );
    calculationSettingsSection.setExpanded( true );

    // Creates Section for "Calculation Elements Unit"
    calculationElementUnitSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    calculationElementUnitSection.setText( "Status der selektierten Berechnungseinheit" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    calculationElementUnitSection.setLayoutData( tableWrapData );
    calculationElementUnitSection.setExpanded( true );

    createCalculationUnit( calculationUnitSection );
    createCalculationSettingsSection( calculationSettingsSection );
    createCalculationElements( calculationElementUnitSection );

    return rootPanel;
  }

  private final void createCalculationUnit( Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );
    sectionFirstComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( sectionFirstComposite );
    FormLayout formLayout = new FormLayout();
    sectionFirstComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    sectionFirstComposite.setLayoutData( formData );
    calcGUI = new CalculationUnitComponent();
    calcGUI.createControl( dataModel, toolkit, sectionFirstComposite );
  }

  private void createCalculationSettingsSection( Section settingsSection )
  {
    settingsSection.setLayout( new FillLayout() );
    sectionSecondComposite = toolkit.createComposite( settingsSection, SWT.FLAT );
    settingsSection.setClient( sectionSecondComposite );

    FormLayout formLayout = new FormLayout();
    sectionSecondComposite.setLayout( formLayout );
    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( sectionFirstComposite, 5 );
    // formData.bottom = new FormAttachment( sectionThirdComposite, -5 );
    sectionSecondComposite.setLayoutData( formData );

    calcComplexSelectionGUI = new CalculationUnitAdministerComponent();
    calcComplexSelectionGUI.createControl( dataModel, toolkit, sectionSecondComposite );
  }

  private void createCalculationElements( Section elementStatusSection )
  {

    elementStatusSection.setLayout( new FillLayout() );
    sectionThirdComposite = toolkit.createComposite( elementStatusSection, SWT.FLAT );
    elementStatusSection.setClient( sectionThirdComposite );
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

  public void disposeControl( )
  {
    if( rootPanel == null )
      return;
    if( !rootPanel.isDisposed() )
    {
      rootPanel.dispose();
      toolkit.dispose();
    }
  }
}