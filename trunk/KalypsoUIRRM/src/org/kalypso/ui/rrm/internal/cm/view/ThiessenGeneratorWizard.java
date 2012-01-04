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
package org.kalypso.ui.rrm.internal.cm.view;

import java.net.URL;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.arguments.Arguments;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.layoutwizard.LayoutWizardPage;

/**
 * @author Gernot Belger
 */
public class ThiessenGeneratorWizard extends Wizard
{
  private static final String URN_PAGE_LAYOUT = "urn:sourceforge:kalypso:hydrology:thiessen:wizard:layout"; //$NON-NLS-1$

  private static final String URN_MAP_GMT = "urn:sourceforge:kalypso:hydrology:thiessen:wizard:mapgmt"; //$NON-NLS-1$

  private static final String URN_TABLE_GTT = "urn:sourceforge:kalypso:hydrology:thiessen:wizard:tablegtt"; //$NON-NLS-1$

  private static final String URN_MAP_TOOLBAR = "toolbar:org.kalypso.model.rrm.ui.thiessen.maptoolbar"; //$NON-NLS-1$

  private final LinearSumBean m_bean;

  public ThiessenGeneratorWizard( final LinearSumBean bean )
  {
    m_bean = bean;
  }

  @Override
  public void addPages( )
  {
    final URL argumentLocation = getContext();

    final Arguments arguments = createPageDefinition();

    final IWizardPage thiessenPage = new LayoutWizardPage( "thiessenPage", argumentLocation, arguments );

    thiessenPage.setTitle( "Thiessen Method" );
    thiessenPage.setDescription( "Please select the timeseries that should be used to generate the catchment model." );

    addPage( thiessenPage );
  }

  private URL getContext( )
  {
    try
    {
      final IContainer currentScenario = ScenarioHelper.getScenarioDataProvider().getScenarioFolder();
      return ResourceUtilities.createQuietURL( currentScenario );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private Arguments createPageDefinition( )
  {
    final Arguments arguments = new Arguments();

    arguments.put( "pageLayout", URN_PAGE_LAYOUT );

    /* MAP */
    final Arguments mapArguments = new Arguments();
    arguments.put( "gisMap.1", mapArguments );

    mapArguments.put( "mapTemplate", URN_MAP_GMT );

    final Arguments mapToolbarArguments = new Arguments();
    mapArguments.put( "mapToolbar", mapToolbarArguments );
    // TODO:
    mapToolbarArguments.put( "uri", URN_MAP_TOOLBAR );

    /* LIST */
    final Arguments listArguments = new Arguments();
    arguments.put( "gisTable.1", listArguments );
    listArguments.put( "tableTemplate", URN_TABLE_GTT );

//    <!-- FEATURE-VIEW -->
//    <arg name="featureView.1">
//      <arg name="featureTemplate" value="project://.model/wizard/pages/10_Ombrometer.gft" />
//    </arg>
//    <!-- Tabelle -->
//    <arg name="zmlNewTable.1">
//      <arg name="zmlTableTemplate" value="project://.model/wizard/pages/10_Tabelle.kot" />
//      <arg name="timeserie1" value="id=N#nameColumn=name#linkColumn=precipitationLink1#nameString=%featureprop% [%axisunit%]" />
//    </arg>
//    <!-- Diagramm -->
//    <arg name="zmlNewDiagram.1">
//      <arg name="zmlDiagramTemplate" value="project://.model/wizard/pages/10_Diagramm.kod" />
//      <arg name="executeCommand_1_1" value="org.kalypso.chart.zml.ui.commands.zoom_pan_maximize?enabled=true" />
//      <arg name="zmlDiagramMenuContribution_1" value="toolbar:org.kalypso.hwv.product.sachsen.zml.diagram.menu.general" />
//      <arg name="zmlDiagramMenuContribution_1a" value="toolbar:org.kalypso.hwv.product.sachsen.zml.diagram.menu.separator1" />
//      <arg name="timeserie1" value="id=N#nameColumn=name#linkColumn=precipitationLink1#nameString=%featureprop% [%axisunit%]" />
//      <arg name="timeserie1nZr" value="id=N_OriZR#linkColumn=precipitationLink1" />
//    </arg>

    return arguments;
  }

  // create gml layer for timeseries (aka stations)
  // - fill all timeseries of current parameter type
  // - group somehow by station
  // - preselect all, that are already referenced in current bean
  // - on every feature change, recalculate thiessen polygons

  @Override
  public boolean performFinish( )
  {
    // TODO: calculate thiessen weights and set to catchments

// final String label = m_bean.getLabel();
// m_bean.setProperty( Feature.QN_DESCRIPTION, label + "X" );

    return true;
  }
}