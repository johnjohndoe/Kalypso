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
package org.kalypso.model.flood.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;

/**
 * @author Gernot Belger
 */
public class PolygonEditingHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final MapView mapView = (MapView) window.getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.model.flood.handlers.PolygonEditingHandler.0" ) ); //$NON-NLS-1$
    final IMapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString( "org.kalypso.model.flood.handlers.PolygonEditingHandler.1" ), Messages.getString( "org.kalypso.model.flood.handlers.PolygonEditingHandler.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    /* Activate theme */
    final IMapModell mapModell = mapPanel.getMapModell();
    final IKalypsoTheme themeToActivate = findTheme( mapModell );
    mapModell.activateTheme( themeToActivate );

    return null;
  }

  private IKalypsoTheme findTheme( final IMapModell mapModell )
  {
    final IKalypsoTheme[] result = new IKalypsoTheme[1];

    final IKalypsoThemeVisitor kalypsoThemeVisitor = new IKalypsoThemeVisitor()
    {
      @Override
      public boolean visit( final IKalypsoTheme theme )
      {
        if( theme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
          final IFeatureType ftype = ft.getFeatureType();
          if( ftype == null )
            return true;

          if( GMLSchemaUtilities.substitutes( ftype, IFloodPolygon.QNAME ) )
          {
            result[0] = theme;
            throw new OperationCanceledException(); // stop visiting
          }
        }

        return true;
      }
    };

    try
    {
      mapModell.accept( kalypsoThemeVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    }
    catch( final OperationCanceledException e )
    {
      // nothing, this should happen
    }

    return result[0];
  }
}
