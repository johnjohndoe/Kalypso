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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * Provides map oriented utility methods.
 * 
 * @author Patrice Congo
 */
public class UtilMap
{
  /**
   * Get First Theme which is showing elements substituable to the specified QName (i.e. substituting it).
   */
  static public IKalypsoFeatureTheme findEditableTheme( IMapModell mapModel, QName editElementQName )
  {
    Assert.throwIAEOnNullParam( mapModel, "mapModel" );
    Assert.throwIAEOnNullParam( editElementQName, "editElementQName" );
    final IKalypsoTheme[] allThemes = mapModel.getAllThemes();
    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ftheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = ftheme.getFeatureType();
        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, editElementQName/* Kalypso1D2DSchemaConstants.WB1D2D_F_NODE */) )
          return ftheme;
      }
    }
    return null;
  }

  /**
   * Get all Themes which are showing elements substituable to the specified QName (i.e. substituting it).
   */
  static public IKalypsoFeatureTheme[] findEditableThemes( final IMapModell mapModel, final QName editElementQName )
  {
    Assert.throwIAEOnNullParam( mapModel, "mapModel" );
    Assert.throwIAEOnNullParam( editElementQName, "editElementQName" );
    final IKalypsoTheme[] allThemes = mapModel.getAllThemes();
    final List<IKalypsoFeatureTheme> foundThemes = new ArrayList<IKalypsoFeatureTheme>( allThemes.length );
    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ftheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = ftheme.getFeatureType();
        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, editElementQName/* Kalypso1D2DSchemaConstants.WB1D2D_F_NODE */) )
          foundThemes.add( ftheme );
      }
    }

    return foundThemes.toArray( new IKalypsoFeatureTheme[foundThemes.size()] );
  }

  /**
   * Find a discretisation model within the mapModel themes.
   * @return The first discretisation model encountered in the list of themes.
   */
  static public IFEDiscretisationModel1d2d findFEModelTheme( final IMapModell mapModel )
  {
    Assert.throwIAEOnNullParam( mapModel, "mapModel" );
    final IKalypsoTheme[] allThemes = mapModel.getAllThemes();
    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ftheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = ftheme.getFeatureType();
        if( featureType == null )
        {
          continue;
        }

        final FeatureList featureList = ftheme.getFeatureList();
        final Feature modelFeature = featureList == null ? null : featureList.getParentFeature();
        if( modelFeature != null )
        {
          final IFEDiscretisationModel1d2d model = (IFEDiscretisationModel1d2d) modelFeature.getAdapter( IFEDiscretisationModel1d2d.class );
          if( model != null )
            return model;
        }
      }
    }
    return null;
  }

  /**
   * Answer whether the theme is showing feature of the given type. This check is made based on substitution
   * 
   * @param featureTheme
   *          the theme to check, must not be null
   * @param featureTypeQName
   *          the type of feature to check whether they can be part of the theme
   */
  static public final boolean isShowingFeatureType( IKalypsoFeatureTheme featureTheme, QName featureTypeQName )
  {
    Assert.throwIAEOnNullParam( featureTheme, "featureTheme" );
    Assert.throwIAEOnNullParam( featureTypeQName, "featureTypeQName" );
    IFeatureType featureType = featureTheme.getFeatureType();
    if( GMLSchemaUtilities.substitutes( featureType, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE ) )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
