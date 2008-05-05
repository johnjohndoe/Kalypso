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
package org.kalypso.ogc.gml;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class UserStylePainter
{
  private final UserStyle m_style;

  private final IFeatureSelectionManager m_selectionManager;

  public UserStylePainter( final UserStyle style, final IFeatureSelectionManager selectionManager )
  {
    m_style = style;
    m_selectionManager = selectionManager;
  }

  public void paintSelected( final GMLWorkspace workspace, final double scale, final GM_Envelope bbox, final FeatureList features, final boolean selected, final IProgressMonitor monitor, final IPaintDelegate delegate ) throws CoreException
  {
    paintFeatureTypeStyles( workspace, scale, bbox, features, selected, monitor, delegate );
  }

  public void paintFeatureTypeStyles( final GMLWorkspace workspace, final Double scale, final GM_Envelope bbox, final FeatureList features, final Boolean selected, final IProgressMonitor monitor, final IPaintDelegate paintDelegate ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Zeichne Display-Elemente", m_style.getFeatureTypeStyles().length );

    final FeatureTypeStyle[] fts = m_style.getFeatureTypeStyles();

    for( final FeatureTypeStyle element : fts )
    {
      final SubMonitor childProgres = progress.newChild( 1 );
      paintFeatureTypeStyle( workspace, scale, bbox, features, selected, childProgres, element, paintDelegate );
      ProgressUtilities.done( childProgres );
    }
  }

  /**
   * @param selected
   *            Whether to paint selected or not-selected features. If <code>null</code>, paint all features.
   */
  private void paintFeatureTypeStyle( final GMLWorkspace workspace, final Double scale, final GM_Envelope bbox, final FeatureList features, final Boolean selected, final IProgressMonitor monitor, final FeatureTypeStyle element, final IPaintDelegate delegate ) throws CoreException
  {
    final QName qname = element.getFeatureTypeName();

    final Rule[] rules = element.getRules();

    final SubMonitor progress = SubMonitor.convert( monitor, rules.length );

    for( final Rule rule : rules )
    {
      final SubMonitor childProgress = progress.newChild( 1 );

      paintRule( workspace, scale, bbox, features, selected, childProgress, rule, qname, delegate );
      ProgressUtilities.done( monitor );
    }
  }

  private void paintRule( final GMLWorkspace workspace, final Double scale, final GM_Envelope bbox, final FeatureList features, final Boolean selected, final IProgressMonitor monitor, final Rule rule, final QName qname, final IPaintDelegate delegate ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Zeichne Rule", 100 );

    final List<?> visibleFeatures = features.query( bbox, null );
    
    ProgressUtilities.worked( progress, 15 );

    final SubMonitor loopProgress = progress.newChild( 85 ).setWorkRemaining( visibleFeatures.size() );

    final Filter filter = rule.getFilter();
    for( final Object o : visibleFeatures )
    {
      final SubMonitor childProgress = loopProgress.newChild( 1 );
      paintFeature( workspace, scale, selected, rule, filter, o, qname, childProgress, delegate );
      ProgressUtilities.done( childProgress );
    }
  }

  private void paintFeature( final GMLWorkspace workspace, final Double scale, final Boolean selected, final Rule rule, final Filter filter, final Object featureOrLink, final QName qname, final IProgressMonitor monitor, final IPaintDelegate delegate ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Zeichne Feature", 100 );

    /* resolve any links */
    final Feature feature = FeatureHelper.getFeature( workspace, featureOrLink );

    /* Check for selection */

    try
    {
      /* Only paint really visible features */
      if( filterFeature( feature, selected, filter ) )
      {
        /* Only paint features which apply to the given qname */
        if( qname == null || GMLSchemaUtilities.substitutes( feature.getFeatureType(), qname ) )
        {
          final Symbolizer[] symbolizers = rule.getSymbolizers();
          for( final Symbolizer symbolizer : symbolizers )
          {
            final DisplayElement displayElement = DisplayElementFactory.buildDisplayElement( feature, symbolizer );
            if( displayElement != null )
            {
              /* does scale apply? */
              if( scale == null || displayElement.doesScaleConstraintApply( scale ) )
                delegate.paint( displayElement, progress.newChild( 100 ) );
            }
          }
        }
      }
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * Determines if a feature should be drawn or now
   * 
   * @param selected
   *            Whether to filter selected or non-selected features. If <code>null</code>, selection is not tested.
   */
  private boolean filterFeature( final Feature feature, final Boolean selected, final Filter filter ) throws FilterEvaluationException
  {
    /* Is selected/unselected ? */
    final boolean featureIsSelected = m_selectionManager.isSelected( feature );

    if( selected != null )
    {
      if( selected && !featureIsSelected )
        return false;

      if( !selected && featureIsSelected )
        return false;
    }

    if( filter == null )
      return true;

    return filter.evaluate( feature );
  }
}