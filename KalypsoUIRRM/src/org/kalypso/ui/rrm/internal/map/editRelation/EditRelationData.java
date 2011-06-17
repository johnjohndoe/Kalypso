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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class EditRelationData extends AbstractModelObject
{
  public static final String PROPERTY_MODIFICATION_MODE = "modificationMode"; //$NON-NLS-1$

  public static final String PROPERTY_INFO_FROM = "infoFrom"; //$NON-NLS-1$

  public static final String PROPERTY_INFO_TO = "infoTo"; //$NON-NLS-1$

  public static final String PROPERTY_PROBLEM_MESSAGE = "problemMessage"; //$NON-NLS-1$

  public static final String PROPERTY_INPUT = "input"; //$NON-NLS-1$

  private final Set<Object> m_checkedElements = new HashSet<Object>();

  public static enum MODE
  {
    ADD(Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.0" )), //$NON-NLS-1$
    REMOVE(Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.1" )); //$NON-NLS-1$

    private final String m_label;

    private MODE( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private MODE m_modificationMode = MODE.ADD;

  private String m_infoFrom = StringUtils.EMPTY;

  private String m_infoTo = StringUtils.EMPTY;

  private String m_problemMessage = StringUtils.EMPTY;

  private Feature m_sourceFeature = null;

  private Feature m_targetFeature = null;

  private EditRelationInput m_input = null;

  public EditRelationInput getInput( )
  {
    return m_input;
  }

  public void setInput( final EditRelationInput input )
  {
    final Object oldValue = m_input;

    m_input = input;

    firePropertyChange( PROPERTY_INPUT, oldValue, m_input );
  }

  public MODE getModificationMode( )
  {
    return m_modificationMode;
  }

  public void setModificationMode( final MODE modificationMode )
  {
    final MODE oldValue = m_modificationMode;

    m_modificationMode = modificationMode;

    firePropertyChange( PROPERTY_MODIFICATION_MODE, oldValue, m_modificationMode );

    /* Reset features */
    setFeatures( null, null );
  }

  public String getInfoFrom( )
  {
    return m_infoFrom;
  }

  public String getInfoTo( )
  {
    return m_infoTo;
  }

  public String getProblemMessage( )
  {
    return m_problemMessage;
  }

  public void setInfoFrom( final String infoFrom )
  {
    final String oldValue = m_infoFrom;

    m_infoFrom = infoFrom;

    firePropertyChange( PROPERTY_INFO_FROM, oldValue, m_infoFrom );
  }

  public void setInfoTo( final String infoTo )
  {
    final String oldValue = m_infoTo;

    m_infoTo = infoTo;

    firePropertyChange( PROPERTY_INFO_TO, oldValue, m_infoTo );
  }

  public void setProblemMessage( final String problemMessage )
  {
    final String oldValue = m_problemMessage;

    m_problemMessage = problemMessage;

    firePropertyChange( PROPERTY_PROBLEM_MESSAGE, oldValue, m_problemMessage );
  }

  public boolean isChecked( final Object element )
  {
    return m_checkedElements.contains( element );
  }

  public void setChecked( final Object element, final boolean checked )
  {
    if( checked )
      m_checkedElements.add( element );
    else
      m_checkedElements.remove( element );
  }

  Object[] getCheckedElements( )
  {
    return m_checkedElements.toArray();
  }

  public org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType[] getCheckedRelations( )
  {
    final List<org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType> result = new ArrayList<org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType>();
    for( final Object element : m_checkedElements )
    {
      if( element instanceof org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType )
        result.add( (org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType) element );
    }
    return result.toArray( new org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType[result.size()] );
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

    final String valid = validateSourceAndTarget();
    if( valid == null )
      setProblemMessage( StringUtils.EMPTY );
    else
      setProblemMessage( valid );

    updateInfoText();
  }

  private String validateSourceAndTarget( )
  {
    final StringBuilder fitProblems = new StringBuilder();
    final List<IEditRelationType> fitList = getFitList( fitProblems );
    if( fitProblems.length() != 0 )
      return fitProblems.toString();

    if( fitList.isEmpty() )
    {
      // not valid but no message
      return StringUtils.EMPTY;
    }

    if( m_sourceFeature == m_targetFeature )
      return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.5" ); //$NON-NLS-1$

    /* Valid */
    return null;
  }

  void updateInfoText( )
  {
    setInfoFrom( getFeatureLabel( m_sourceFeature ) );
    setInfoTo( getFeatureLabel( m_targetFeature ) );
  }

  private static String getFeatureLabel( final Feature feature )
  {
    if( feature == null )
      return "<none>";
    else
      return FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
  }

  /**
   * @see LightRelationType that fit to the selected features
   */
  public List<IEditRelationType> getFitList( final StringBuilder fitProblems )
  {
    final List<IEditRelationType> fitList = new ArrayList<IEditRelationType>();

    if( m_input == null || !(m_input instanceof IKalypsoFeatureTheme) )
      return fitList;

    if( m_sourceFeature == null || m_targetFeature == null )
      return fitList;

    final IFeatureType sourceFT = m_sourceFeature.getFeatureType();
    final IFeatureType targetFT = m_targetFeature.getFeatureType();

    final GMLWorkspace workspace = ((IKalypsoFeatureTheme) m_input).getWorkspace();
    final IEditRelationType[] relations = getCheckedRelations();
    final MODE mode = getModificationMode();
    for( final IEditRelationType relation : relations )
    {
      if( relation.fitsTypes( sourceFT, targetFT ) )
      {
        final String problem = relation.getFitProblems( workspace, m_sourceFeature, m_targetFeature, mode == MODE.ADD );
        if( problem == null )
          fitList.add( relation );
        else
          fitProblems.append( problem );
      }
    }
    if( fitList.isEmpty() && fitProblems.length() == 0 )
      fitProblems.append( Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.4" ) ); //$NON-NLS-1$
    return fitList;
  }
}