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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckInEventData;
import org.kalypso.model.wspm.pdb.wspm.CheckinCalculationOperation;
import org.kalypso.model.wspm.pdb.wspm.SaveEventOperation;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFactory;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.ui.export.ProfileResultExportPage;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckinCalculationWorker implements ICheckInWorker
{
  private final CheckInEventData<CalculationWspmTuhhSteadyState> m_data;

  private ProfileResultExportPage m_resultPage;

  public CheckinCalculationWorker( final CommandableWorkspace workspace, final CalculationWspmTuhhSteadyState calculation )
  {
    m_data = new CheckInEventData<CalculationWspmTuhhSteadyState>( workspace, calculation )
    {
      @Override
      public WaterBody findWaterBody( )
      {
        final Map<String, WaterBody> existingWaterCodes = getWaterHash();

        final WspmWaterBody wspmWaterBody = calculation.getReach().getWaterBody();
        final String waterCode = wspmWaterBody.getRefNr();

        return existingWaterCodes.get( waterCode );
      }
    };

    m_data.getEvent().setType( TYPE.Simulation );
  }

  @Override
  public void preInit( final IPdbConnection connection ) throws PdbConnectException
  {
    m_data.init( connection );
  }

  @Override
  public IStatus checkPreconditions( )
  {
    final CalculationWspmTuhhSteadyState calculation = m_data.getWspmObject();

    /* only certain calculation can check in...! */
    final TuhhReach reach = calculation.getReach();
    if( reach == null )
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "CheckinCalculationWorker.0" ) ); //$NON-NLS-1$

    final MODE calcMode = calculation.getCalcMode();
    if( calcMode != MODE.WATERLEVEL )
    {
      final IValuePropertyType modeProperty = (IValuePropertyType)calculation.getFeatureType().getProperty( TuhhCalculation.PROPERTY_MODE );
      final Map<Object, String> labelHash = PropertyUtils.createComboEntries( modeProperty );
      final String label = labelHash.get( MODE.WATERLEVEL.name() );

      final String message = String.format( Messages.getString( "CheckinCalculationWorker.1" ), label ); //$NON-NLS-1$

      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    /* Water Body must exist */
    final WaterBody waterBody = m_data.findWaterBody();
    if( waterBody == null )
    {
      final WspmWaterBody wspmWaterBody = calculation.getReach().getWaterBody();
      final String waterCode = wspmWaterBody.getRefNr();

      final String waterName = wspmWaterBody.getName();
      final String message = CheckInEventWorker.formatMissingWaterBody( waterCode, waterName );
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    return Status.OK_STATUS;
  }

  @Override
  public Wizard createWizard( )
  {
    final CheckInEventWizard checkInEventWizard = new CheckInEventWizard( this, m_data );

    final IWspmResultNode results = WspmResultFactory.createResultNode( null, m_data.getWspmObject() );

    // FIXME: make sure that exactly only one result can be selected
    m_resultPage = new ProfileResultExportPage( "profileResults", results, true ); //$NON-NLS-1$
    m_resultPage.setShowComponentChooser( false );

    checkInEventWizard.addPage( m_resultPage );

    return checkInEventWizard;
  }

  @Override
  public void configureSelector( final ElementSelector selector )
  {
    selector.addEventId( m_data.getEvent().getId() );
  }

  @Override
  public void closeConnection( )
  {
    m_data.closeConnection();
  }

  @Override
  public boolean performFinish( final IWizardContainer container )
  {
    if( !SaveEventOperation.askForEmptyState( m_data.getEvent(), container.getShell(), container.getCurrentPage().getWizard().getWindowTitle() ) )
      return false;

    final CheckinCalculationOperation operation = new CheckinCalculationOperation( m_data );

    final WspmResultLengthSection[] lengthSections = m_resultPage.getSelectedLengthSections();
    if( lengthSections.length > 0 )
      operation.setLengthSections( lengthSections[0] );

    return CheckInEventWorker.executeOnContainer( container, operation );
  }
}