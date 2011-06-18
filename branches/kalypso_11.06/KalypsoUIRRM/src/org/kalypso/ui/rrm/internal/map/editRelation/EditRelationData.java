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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * @author Gernot Belger
 */
public class EditRelationData extends AbstractModelObject
{
  public static final String PROPERTY_MODIFICATION_MODE = "modificationMode"; //$NON-NLS-1$

  public static final String PROPERTY_INPUT = "input"; //$NON-NLS-1$

  private EditRelationMode m_modificationMode = EditRelationMode.ADD;

  private Feature m_sourceFeature = null;

  private Feature m_targetFeature = null;

  private EditRelationInput m_input = null;

  private FeatureList m_allowedSourceFeatures = null;

  private FeatureList m_allowedTargetFeatures = null;

  private IEditRelationType m_relation;

  public EditRelationInput getInput( )
  {
    return m_input;
  }

  public void setInput( final EditRelationInput input )
  {
    final Object oldValue = m_input;

    m_input = input;

    firePropertyChange( PROPERTY_INPUT, oldValue, m_input );

    recalculateAllowedFeatures();
  }

  public EditRelationMode getModificationMode( )
  {
    return m_modificationMode;
  }

  public void setModificationMode( final EditRelationMode modificationMode )
  {
    final EditRelationMode oldValue = m_modificationMode;

    m_modificationMode = modificationMode;

    firePropertyChange( PROPERTY_MODIFICATION_MODE, oldValue, m_modificationMode );

    /* Reset features */
    setFeatures( null, null );
  }

  public void setRelation( final IEditRelationType relation )
  {
    m_relation = relation;
  }

  public IEditRelationType getRelation( )
  {
    return m_relation;
  }

  public Feature getSourceFeature( )
  {
    return m_sourceFeature;
  }

  public Feature getTargetFeature( )
  {
    return m_targetFeature;
  }

  public void setFeatures( final Feature sourceFeature, final Feature targetFeature )
  {
    m_sourceFeature = sourceFeature;
    m_targetFeature = targetFeature;
  }

  public FeatureList getAllowedSourceFeatures( )
  {
    return m_allowedSourceFeatures;
  }

  public FeatureList getAllowedTargetFeatures( )
  {
    return m_allowedTargetFeatures;
  }

  void recalculateAllowedFeatures( )
  {
    m_allowedSourceFeatures = null;
    m_allowedTargetFeatures = null;

    if( m_input == null || m_relation == null )
      return;

    final NaModell naModel = m_input.getNaModel();
    final IEditRelationType relation = getRelation();

    final FindAllowedFeaturesVisitor visitor = new FindAllowedFeaturesVisitor( relation );
    m_input.getWorkspace().accept( visitor, naModel, FeatureVisitor.DEPTH_INFINITE );

    m_allowedSourceFeatures = visitor.getSourceFeatures();
    m_allowedTargetFeatures = visitor.getTargetFeatures();
  }
}