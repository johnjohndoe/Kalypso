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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class TransitionElement extends AbstractFeatureBinder implements ITransitionElement
{
  private FeatureWrapperCollection<IFELine> m_continuityLines;

  private ITransitionElement.TRANSITION_TYPE m_transition_type;

  public TransitionElement( final Feature featureToBind )
  {
    this( featureToBind, ITransitionElement.QNAME );
  }

  public TransitionElement( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    // m_continuityLines = (List<IFELine>) featureToBind.getProperty( ITransitionElement.PROP_CONTI_LINES );
    final Object prop = featureToBind.getProperty( ITransitionElement.PROP_CONTI_LINES );
    if( prop == null )
      m_continuityLines = new FeatureWrapperCollection<IFELine>( featureToBind, ITransitionElement.QNAME, ITransitionElement.PROP_CONTI_LINES, IFELine.class );
    else
      m_continuityLines = new FeatureWrapperCollection<IFELine>( featureToBind, IFELine.class, ITransitionElement.PROP_CONTI_LINES );
    final Object property = getFeature().getProperty( ITransitionElement.PROP_TRANSITION_TYPE );
    if( property == null )
      m_transition_type = TRANSITION_TYPE.TYPE1D2D;
    else
    {
      if( TRANSITION_TYPE.TYPE2D1D.getValue().equals( property ) )
        m_transition_type = TRANSITION_TYPE.TYPE2D1D;
      else
        // type1D2D is default value... 
        m_transition_type = TRANSITION_TYPE.TYPE1D2D;
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#getContinuityLine()
   */
  @Override
  public List<IFELine> getContinuityLines( )
  {
    return m_continuityLines;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#recalculateElementGeometry()
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    if( m_continuityLines.size() < 2 )
      return null;
    final GM_Position[] positions0 = m_continuityLines.get( 0 ).getGeometry().getAsLineString().getPositions();
    final GM_Position[] positions1 = m_continuityLines.get( 1 ).getGeometry().getAsLineString().getPositions();

    final GM_Position[] positions = new GM_Position[positions0.length + positions1.length + 1];
    for( int i = 0; i < positions0.length; i++ )
      positions[i] = positions0[i];

    // check if polygon created from these lines is self-overlapped; if it is, get second line points in opposite order
    final double distanceToFirst = positions0[positions0.length - 1].getDistance( positions1[0] );
    final double distanceToLast = positions0[positions0.length - 1].getDistance( positions1[positions1.length - 1] );
    int k = positions0.length;
    if( distanceToFirst < distanceToLast )
      for( final GM_Position element : positions1 )
        positions[k++] = element;
    else
      for( int i = positions1.length - 1; i >= 0; i-- )
        positions[k++] = positions1[i];

    // close the ring
    positions[positions.length - 1] = positions0[0];
    return GeometryFactory.createGM_Surface( positions, new GM_Position[][] {}, m_continuityLines.get( 0 ).getGeometry().getCoordinateSystem() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#addElementAsRef(org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem)
   */
  @Override
  public boolean addElementAsRef( final IFENetItem element )
  {
    return m_continuityLines.addRef( (IFELine) element );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#getElements()
   */
  @Override
  public IFeatureWrapperCollection<IFENetItem> getElements( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#removeElementAsRef(org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem)
   */
  @Override
  public void removeElementAsRef( final IFENetItem elment )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement#isMemberOfCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public boolean isMemberOfCalculationUnit( final ICalculationUnit calculationUnit )
  {
    if( calculationUnit instanceof ICalculationUnit1D2D )
    {
      final List<IFELine> calcUnitContinuityLines = calculationUnit.getContinuityLines();
      boolean allLinesFound = true;
      boolean lineFound = false;
      for( final IFELine myLine : m_continuityLines )
      {
        if( !allLinesFound )
          break;
        lineFound = false;
        final String myLineGmlID = myLine.getGmlID();
        for( final IFELine calcUnitLine : calcUnitContinuityLines )
        {
          if( calcUnitLine.getGmlID().equals( myLineGmlID ) )
          {
            lineFound = true;
            break;
          }
        }
        allLinesFound &= lineFound;
      }
      return allLinesFound;
    }
    return false;
  }

  @Override
  public ITransitionElement.TRANSITION_TYPE getTransitionType( )
  {
    return m_transition_type;
  }

  @Override
  public void setTransitionType( final ITransitionElement.TRANSITION_TYPE transition_type )
  {
    m_transition_type = transition_type;
    getFeature().setProperty( ITransitionElement.PROP_TRANSITION_TYPE, m_transition_type.getValue() );
  }

}
