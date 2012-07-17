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
package org.kalypso.ui.rrm.internal.cm.idw;

import java.net.URL;
import java.util.Locale;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.arguments.Arguments;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.layoutwizard.ILayoutWizardPage;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.TimeseriesValidatingOperation;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ui.layoutwizard.LayoutWizardPage;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class IdwGeneratorWizard extends Wizard
{
  private static final String URN_PAGE_LAYOUT = "urn:sourceforge:kalypso:hydrology:idw:wizard:layout"; //$NON-NLS-1$

  private static final String URN_MAP_GMT = "urn:sourceforge:kalypso:hydrology:idw:wizard:mapgmt"; //$NON-NLS-1$

  private static final String URN_TABLE_GTT = "urn:sourceforge:kalypso:hydrology:idw:wizard:tablegtt"; //$NON-NLS-1$

  private static final String URN_THIESSEN_GFT = "urn:sourceforge:kalypso:hydrology:idw:wizard:featureviewgft"; //$NON-NLS-1$

  private static final String URN_MAP_TOOLBAR = "toolbar:org.kalypso.model.rrm.ui.idw.maptoolbar"; //$NON-NLS-1$

  private final LinearSumBean m_bean;

  public IdwGeneratorWizard( final LinearSumBean bean )
  {
    m_bean = bean;

    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() ) );
  }

  @Override
  public void addPages( )
  {
    final URL argumentLocation = getContext();
    final Arguments arguments = createPageDefinition();
    final IWizardPage idwPage = new LayoutWizardPage( "idwPage", argumentLocation, arguments ); //$NON-NLS-1$

    idwPage.setTitle( Messages.getString( "IdwGeneratorWizard_1" ) ); //$NON-NLS-1$
    idwPage.setDescription( Messages.getString( "IdwGeneratorWizard_2" ) ); //$NON-NLS-1$

    addPage( idwPage );
  }

  @Override
  public boolean performCancel( )
  {
    /* Save data in order to avoid the message that a gml should be saved. */
    final IWizardPage[] pages = getPages();
    for( final IWizardPage page : pages )
    {
      if( page instanceof ILayoutWizardPage )
        ((ILayoutWizardPage) page).saveData( true, new NullProgressMonitor() );
    }

    return super.performCancel();
  }

  @Override
  public boolean performFinish( )
  {
    /* Check the validity range. */
    final ITimeseries[] timeseries = LinearSumHelper.collectTimeseries( m_bean );
    final DateRange dateRange = LinearSumHelper.createDateRange( m_bean );
    final TimeseriesValidatingOperation timeseriesOperation = new TimeseriesValidatingOperation( timeseries, dateRange );
    final IStatus timeseriesStatus = timeseriesOperation.execute( new NullProgressMonitor() );
    if( !timeseriesStatus.isOK() )
    {
      final MultiStatus status = new MultiStatus( KalypsoUIRRMPlugin.getID(), IStatus.ERROR, new IStatus[] { timeseriesStatus }, Messages.getString( "EditLinearSumDialog.0" ), null ); //$NON-NLS-1$
      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getShell().getText() );
      statusDialog.open();
      return false;
    }

    /* Set the comment. */
    final Integer maxNumberStations = getMaxNumberStations();
    m_bean.setProperty( ILinearSumGenerator.PROPERTY_COMMENT, String.format( Locale.PRC, Messages.getString( "IdwFactorsOperation_0" ), maxNumberStations ) ); //$NON-NLS-1$

    final IWizardPage[] pages = getPages();
    final IdwFactorsOperation operation = new IdwFactorsOperation( maxNumberStations, pages, m_bean );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    return !status.matches( IStatus.ERROR );
  }

  private Integer getMaxNumberStations( )
  {
    final ILayoutWizardPage wizardPage = (ILayoutWizardPage) getPages()[0];
    final IdwOptionsLayoutPart optionsPart = (IdwOptionsLayoutPart) wizardPage.findLayoutPart( "idwOptionsPart.1" ); //$NON-NLS-1$
    final Integer maxStations = optionsPart.getMaxStations();
    if( maxStations == null || maxStations.intValue() == 0 )
      return 1;
    //throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "IdwFactorsOperation_5" ) ) ); //$NON-NLS-1$

    return maxStations;
  }

  private Arguments createPageDefinition( )
  {
    /* Map toolbar arguments. */
    final Arguments mapToolbarArguments = new Arguments();
    mapToolbarArguments.put( "uri", URN_MAP_TOOLBAR ); //$NON-NLS-1$

    /* Map arguments. */
    final Arguments mapArguments = new Arguments();
    mapArguments.put( "mapTemplate", URN_MAP_GMT ); //$NON-NLS-1$
    mapArguments.put( "mapToolbar", mapToolbarArguments ); //$NON-NLS-1$

    /* List arguments. */
    final Arguments listArguments = new Arguments();
    listArguments.put( "tableTemplate", URN_TABLE_GTT ); //$NON-NLS-1$

    /* Feature view arguments. */
    final Arguments featureviewArguments = new Arguments();
    featureviewArguments.put( "featureTemplate", URN_THIESSEN_GFT ); //$NON-NLS-1$

    /* General arguments. */
    final Arguments arguments = new Arguments();
    arguments.put( "pageLayout", URN_PAGE_LAYOUT ); //$NON-NLS-1$
    arguments.put( "gisMap.1", mapArguments ); //$NON-NLS-1$
    arguments.put( "gisTable.1", listArguments ); //$NON-NLS-1$
    arguments.put( "featureView.1", featureviewArguments ); //$NON-NLS-1$

    return arguments;
  }

  private URL getContext( )
  {
    final IContainer currentScenario = KalypsoAFGUIFrameworkPlugin.getDataProvider().getScenarioFolder();
    return ResourceUtilities.createQuietURL( currentScenario );
  }

  LinearSumBean getGenerator( )
  {
    return m_bean;
  }
}