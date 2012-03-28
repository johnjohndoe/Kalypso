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
package org.kalypso.ui.rrm.internal.gml.feature.view;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.services.IEvaluationService;
import org.joda.time.Period;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.gml.feature.view.dialogs.ChooseTimeseriesDialog;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypso.zml.ui.imports.ParameterTypeLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * @author Dirk Kuch
 */
public class ChooseZmlLinkFeatureViewControl extends AbstractFeatureControl
{
  protected final String m_parameterType;

  private Text m_text;

  /**
   * @param parameterType
   *          display only timeseries of this type
   */
  public ChooseZmlLinkFeatureViewControl( final Feature feature, final IPropertyType ftp, final String parameterType )
  {
    super( feature, ftp );
    m_parameterType = parameterType;
  }

  @Override
  public Control createControl( final FormToolkit toolkit, final Composite parent, final int style )
  {
    final Composite body = new Composite( parent, SWT.NULL );
    body.setLayout( Layouts.createGridLayout( 2 ) );

    m_text = new Text( body, SWT.BORDER | SWT.READ_ONLY );
    m_text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Button button = new Button( body, SWT.PUSH );
    button.setText( "..." ); //$NON-NLS-1$

    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final CommandableWorkspace workspace = getStationsWorkspace();
        final IStationCollection collection = getStationCollection();

        if( Objects.isNull( workspace, collection ) )
        {
          MessageDialog.openError( button.getShell(), Messages.getString( "ChooseZmlLinkFeatureViewControl_0" ), Messages.getString( "ChooseZmlLinkFeatureViewControl_1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return;
        }

        final ChooseTimeseriesDialog dialog = new ChooseTimeseriesDialog( button.getShell(), workspace, collection, m_parameterType );
        dialog.setSelection( getTimeseries() );

        if( dialog.open() == Window.OK )
        {
          final ITimeseries selection = dialog.getSelection();
          doSelectionChanged( selection );
        }
      }

    } );

    updateControl();

    return body;
  }

  protected void doSelectionChanged( final ITimeseries selection )
  {
    final ZmlLink link = selection.getDataLink();

    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();

    final FeatureChange change = new FeatureChange( feature, getFeatureTypeProperty(), link.getTimeseriesLink() );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { change } );

    fireFeatureChange( command );
  }

  protected CommandableWorkspace getStationsWorkspace( )
  {
    try
    {
      final IEvaluationService service = (IEvaluationService) PlatformUI.getWorkbench().getService( IEvaluationService.class );
      final IEvaluationContext context = service.getCurrentState();

      final SzenarioDataProvider modelProvider = (SzenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final CommandableWorkspace workspace = modelProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      return workspace;
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return null;
  }

  protected IStationCollection getStationCollection( )
  {
    try
    {
      final IEvaluationService service = (IEvaluationService) PlatformUI.getWorkbench().getService( IEvaluationService.class );
      final IEvaluationContext context = service.getCurrentState();

      final SzenarioDataProvider modelProvider = (SzenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IStationCollection collection = modelProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      return collection;
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return null;
  }

  @Override
  public void updateControl( )
  {
    final ITimeseries timeseries = getTimeseries();

    if( Objects.isNull( timeseries ) )
    {
      m_text.setText( Messages.getString( "ChooseZmlLinkFeatureViewControl_2" ) ); //$NON-NLS-1$
      return;
    }

    m_text.setText( toLabel( timeseries ) );
  }

  private ITimeseries getTimeseries( )
  {
    final Object objLink = getFeature().getProperty( getFeatureTypeProperty() );
    if( !(objLink instanceof TimeseriesLinkType) )
      return null;

    final TimeseriesLinkType link = (TimeseriesLinkType) objLink;
    final IStationCollection collection = getStationCollection();

    final FindTimeseriesLinkRunnable runnable = new FindTimeseriesLinkRunnable( collection, link );
    runnable.execute( new NullProgressMonitor() );

    return runnable.getTimeseries();
  }

  private String toLabel( final ITimeseries timeseries )
  {
    final String station = timeseries.getDescription();
    final String type = timeseries.getParameterType();
    final Period timestep = timeseries.getTimestep();
    final String quality = timeseries.getQuality();

    final StringBuffer buffer = new StringBuffer();

    if( StringUtils.isNotEmpty( station ) )
    {
      buffer.append( String.format( Messages.getString( "ChooseZmlLinkFeatureViewControl_3" ), station ) ); //$NON-NLS-1$
    }

    if( Objects.isNotNull( timestep ) )
    {
      final String dateString = PeriodUtils.formatDefault( timestep );
      buffer.append( String.format( Messages.getString( "ChooseZmlLinkFeatureViewControl_5" ), dateString ) ); //$NON-NLS-1$
    }

    if( StringUtils.isNotEmpty( quality ) )
    {
      buffer.append( String.format( Messages.getString( "ChooseZmlLinkFeatureViewControl_6" ), quality ) ); //$NON-NLS-1$
    }

    if( StringUtils.isNotEmpty( type ) )
    {
      final ParameterTypeLabelProvider provider = new ParameterTypeLabelProvider();
      buffer.append( String.format( " (%s)", provider.getText( type ) ) );
    }

    return buffer.toString().trim();
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }
}