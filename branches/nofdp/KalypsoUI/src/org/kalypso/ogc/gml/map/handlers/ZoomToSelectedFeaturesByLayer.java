/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.map.handlers;

import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.handlers.parts.ZoomToFeaturesPart;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * This class is a handler for zooming to selected features of one layer.
 * 
 * @author Holger Albert
 */
public class ZoomToSelectedFeaturesByLayer extends AbstractHandler
{
  /**
   * The theme which should be used.
   */
  private IKalypsoTheme m_theme;

  /**
   * Percentage for increasing the new extend.
   */
  private int m_percent;

  /**
   * The constructor.
   */
  public ZoomToSelectedFeaturesByLayer( )
  {
    m_theme = null;
    m_percent = 5;
  }

  /**
   * The constructor.
   * 
   * @param theme
   *          The theme which should be used.
   * @param percent
   *          Percentage for increasing the new extend.
   */
  public ZoomToSelectedFeaturesByLayer( IKalypsoTheme theme, int percent )
  {
    m_theme = theme;

    m_percent = 5;
    if( percent > 0 )
      m_percent = percent;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( ExecutionEvent event ) throws ExecutionException
  {
    IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( part == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToSelectedFeaturesByLayer.0") ); //$NON-NLS-1$

    MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
    if( mapPanel == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToSelectedFeaturesByLayer.1") ); //$NON-NLS-1$

    /* If no theme is given, use the active theme. */
    if( m_theme == null )
      m_theme = mapPanel.getMapModell().getActiveTheme();

    /* Collect the selected features of this theme. */
    ArrayList<Feature> features = new ArrayList<Feature>();

    /* This list consists of all selected features. */
    EasyFeatureWrapper[] allFeatures = mapPanel.getSelectionManager().getAllFeatures();

    /* At the moment only the gis template feature theme is supported. */
    if( m_theme instanceof GisTemplateFeatureTheme )
    {
      GisTemplateFeatureTheme theme = (GisTemplateFeatureTheme) m_theme;
      FeatureList featureList = theme.getFeatureList();

      for( int i = 0; i < featureList.size(); i++ )
      {
        Object object = featureList.get( i );

        /* Only features are supported. */
        if( object instanceof Feature )
        {
          Feature feature = (Feature) object;

          /* If this feature of the theme is one of the selecteded, add it. */
          for( int j = 0; j < allFeatures.length; j++ )
          {
            Feature selectedFeature = allFeatures[j].getFeature();
            if( selectedFeature.equals( feature ) )
            {
              features.add( feature );
              break;
            }
          }
        }
      }
    }

    if( features.size() == 0 )
      return Status.OK_STATUS;

    /* Create the piece, which will zoom to the features. */
    ZoomToFeaturesPart piece = new ZoomToFeaturesPart( part, features, m_percent );

    /* Zoom to the features. */
    piece.zoomTo();

    return Status.OK_STATUS;
  }
}