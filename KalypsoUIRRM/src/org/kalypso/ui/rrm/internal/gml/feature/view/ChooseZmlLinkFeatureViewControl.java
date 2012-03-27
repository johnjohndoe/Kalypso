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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.dialogs.MessageDialog;
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
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.gml.feature.view.dialogs.ChooseTimeseriesDialog;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * @author Dirk Kuch
 */
public class ChooseZmlLinkFeatureViewControl extends AbstractFeatureControl
{

  private final String m_parameterType;

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

    final Text text = new Text( body, SWT.BORDER | SWT.READ_ONLY );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

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
          MessageDialog.openError( button.getShell(), "Error", "Can't resolve stations.gml" );
          return;
        }

        final ChooseTimeseriesDialog dialog = new ChooseTimeseriesDialog( button.getShell(), workspace, collection, m_parameterType );
        dialog.open();

      }

    } );

    return body;
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
// final IStatus status = validateStorageChannel();
// m_statusComposite.setStatus( status );
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }
}