/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.AbstractFeatureSelection;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public final class KalypsoFeatureThemeSelection extends AbstractFeatureSelection
{
  private final CommandableWorkspace m_workspace;

  private final Feature m_parentFeature;

  private final IRelationType m_parentPropertyName;

  private final IFeatureSelectionManager m_selectionManager;

  private final Feature m_focusedFeature;

  private final IPropertyType m_focusedProperty;

  public KalypsoFeatureThemeSelection( final List<Feature> selectedFeatures, final IKalypsoFeatureTheme filterTheme, final IFeatureSelectionManager selectionManager, final Feature focusedFeature, final IPropertyType focusedProperty )
  {
    super( filter( selectedFeatures, filterTheme ) );

    m_selectionManager = selectionManager;
    m_focusedFeature = focusedFeature;
    m_focusedProperty = focusedProperty;
    m_workspace = filterTheme.getWorkspace();
    final FeatureList featureList = filterTheme.getFeatureList();
    m_parentFeature = featureList == null ? null : featureList.getParentFeature();

    m_parentPropertyName = featureList == null ? null : featureList.getParentFeatureTypeProperty();
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getWorkspace(org.kalypsodeegree.model.feature.Feature)
   */
  public CommandableWorkspace getWorkspace( final Feature feature )
  {
    return m_workspace;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeature(org.kalypsodeegree.model.feature.Feature)
   */
  public Feature getParentFeature( final Feature feature )
  {
    return m_parentFeature;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeatureProperty(org.kalypsodeegree.model.feature.Feature)
   */
  public IRelationType getParentFeatureProperty( final Feature feature )
  {
    return m_parentPropertyName;
  }

  /** Return a new selection wich contains all features from the given selection wich are contained in the theme. */
  public static IStructuredSelection filter( final List<Feature> selection, final IKalypsoFeatureTheme theme )
  {
    // TODO: only visible features?
    final FeatureList featureList = theme.getFeatureList();
    if( featureList == null )
      return StructuredSelection.EMPTY;

    // TODO: major performance bug! Calls the slow contains method on split-sort
    final ArrayList<Feature> list = new ArrayList<Feature>( selection );
    list.retainAll( featureList );
    return new StructuredSelection( list );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getAllFeatures()
   */
  public EasyFeatureWrapper[] getAllFeatures( )
  {
    return FeatureSelectionHelper.createEasyWrappers( this );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getSelectionManager()
   */
  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedFeature()
   */
  public Feature getFocusedFeature( )
  {
    return m_focusedFeature;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedProperty()
   */
  public IPropertyType getFocusedProperty( )
  {
    return m_focusedProperty;
  }
}