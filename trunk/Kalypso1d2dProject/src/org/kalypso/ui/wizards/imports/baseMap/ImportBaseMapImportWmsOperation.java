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
package org.kalypso.ui.wizards.imports.baseMap;

import org.deegree.ogcwebservices.wms.capabilities.Layer;
import org.deegree.ogcwebservices.wms.capabilities.Style;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ui.action.AddThemeCommand;

/**
 * @author Gernot Belger
 */
public class ImportBaseMapImportWmsOperation implements IImportBaseMapOperation
{
  private final ICommandTarget m_cmdTarget;

  private final IKalypsoLayerModell m_mapModell;

  private final ImportBaseMapImportWmsPage m_wmsPage;

  public ImportBaseMapImportWmsOperation( final ImportBaseMapImportWmsPage wmsPage, final ICommandTarget cmdTarget, final IKalypsoLayerModell mapModell )
  {
    m_wmsPage = wmsPage;
    m_cmdTarget = cmdTarget;
    m_mapModell = mapModell;
  }

  @Override
  public boolean checkPreconditions( final Shell shell, final String windowTitle )
  {
    // nothing to do
    return true;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    /* Finishes the work on this page (dialog settings). */
    m_wmsPage.finish();

    // FIXME: mega ugly and most pobably copy paste from elswere!

    if( m_wmsPage.isMultiLayer() )
    {
      final StringBuffer source = new StringBuffer( IKalypsoImageProvider.KEY_URL + "=" + m_wmsPage.getBaseURL().toString() ); //$NON-NLS-1$
      final StringBuffer layers = new StringBuffer( IKalypsoImageProvider.KEY_LAYERS + "=" ); //$NON-NLS-1$
      final StringBuffer styles = new StringBuffer( IKalypsoImageProvider.KEY_STYLES + "=" ); //$NON-NLS-1$
      final StringBuffer provider = new StringBuffer( IKalypsoImageProvider.KEY_PROVIDER + "=" ); //$NON-NLS-1$

      final Layer[] layerArray = m_wmsPage.getLayersList();
      for( int i = 0; i < layerArray.length; i++ )
      {
        final Layer layer = layerArray[i];
        final String layerName = layer.getName();
        final String styleName;
        final Style[] styles2 = layer.getStyles();
        if( styles2.length > 0 )
          styleName = styles2[0].getName();
        else
          styleName = "default"; //$NON-NLS-1$
        layers.append( layerName );
        styles.append( styleName );
        if( i < layerArray.length - 1 )
        {
          layers.append( "," ); //$NON-NLS-1$
          styles.append( "," ); //$NON-NLS-1$
        }
      }

      final String providerID = m_wmsPage.getProviderID();
      if( providerID != null )
        provider.append( providerID );

      final String layerName = "Multi" + source; //$NON-NLS-1$
      source.append( "#" ).append( layers.toString() ); //$NON-NLS-1$
      source.append( "#" ).append( styles.toString() ); //$NON-NLS-1$
      source.append( "#" ).append( provider.toString() ); //$NON-NLS-1$

      final AddThemeCommand command = new AddThemeCommand( m_mapModell, layerName, "wms", source.toString() ); //$NON-NLS-1$
      m_cmdTarget.postCommand( command, null );
      return Status.OK_STATUS;
    }
    else
    {
      final Layer[] layerArray = m_wmsPage.getLayersList();
      for( final Layer layer : layerArray )
      {
        final StringBuffer source = new StringBuffer( IKalypsoImageProvider.KEY_URL + "=" + m_wmsPage.getBaseURL().toString() ); //$NON-NLS-1$

        final String layerName = layer.getName();
        final String styleName;
        final Style[] styles2 = layer.getStyles();
        if( styles2.length > 0 )
          styleName = styles2[0].getName();
        else
          styleName = "default"; //$NON-NLS-1$

        String providerID = m_wmsPage.getProviderID();
        if( providerID == null )
          providerID = ""; //$NON-NLS-1$

        final String layerTitle = layer.getTitle();
        source.append( "#" ).append( IKalypsoImageProvider.KEY_LAYERS ).append( "=" ).append( layerName ); //$NON-NLS-1$ //$NON-NLS-2$
        source.append( "#" ).append( IKalypsoImageProvider.KEY_STYLES ).append( "=" ).append( styleName ); //$NON-NLS-1$ //$NON-NLS-2$
        source.append( "#" ).append( IKalypsoImageProvider.KEY_PROVIDER ).append( "=" ).append( providerID ); //$NON-NLS-1$ //$NON-NLS-2$

        final AddThemeCommand command = new AddThemeCommand( m_mapModell, layerTitle, "wms", source.toString() ); //$NON-NLS-1$
        m_cmdTarget.postCommand( command, null );
      }

      return Status.OK_STATUS;
    }
  }
}