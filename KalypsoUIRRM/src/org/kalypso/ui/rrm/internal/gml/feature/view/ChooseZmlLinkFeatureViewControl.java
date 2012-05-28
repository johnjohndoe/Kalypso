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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.gml.feature.view.dialogs.ChooseTimeseriesDialog;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

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
        final CommandableWorkspace workspace = FindTimeseriesLinkRunnable.getStationsWorkspace();
        final IStationCollection collection = FindTimeseriesLinkRunnable.getStationCollection();

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
          final ZmlLink link = selection == null ? null : selection.getDataLink();
          final TimeseriesLinkType linkType = link == null ? null : link.getTimeseriesLink();

          final Feature feature = getFeature();

          final FeatureChange change = new FeatureChange( feature, getFeatureTypeProperty(), linkType );
          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( feature.getWorkspace(), new FeatureChange[] { change } );

          fireFeatureChange( command );
        }
      }
    } );

    updateControl();

    return body;
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

    m_text.setText( Timeserieses.toLinkLabel( timeseries ) );
  }

  protected ITimeseries getTimeseries( )
  {
    final Object objLink = getFeature().getProperty( getFeatureTypeProperty() );
    if( !(objLink instanceof TimeseriesLinkType) )
      return null;

    final TimeseriesLinkType link = (TimeseriesLinkType) objLink;
    final String href = link.getHref();

    return FindTimeseriesLinkRunnable.findTimeseries( href );
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }
}