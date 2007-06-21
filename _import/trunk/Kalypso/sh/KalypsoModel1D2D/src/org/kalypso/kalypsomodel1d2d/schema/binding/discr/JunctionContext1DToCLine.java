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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default implementation of {@link IJunctionContext1DToCLine}
 * 
 * @author Patrice Congo
 */
@SuppressWarnings({"unchecked", "hiding"})
public class JunctionContext1DToCLine 
                  extends AbstractFeatureBinder 
                  implements IJunctionContext1DToCLine
{

  public JunctionContext1DToCLine( Feature featureToBind )
  {
    this(
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_CLINE);
  }
  public JunctionContext1DToCLine( 
                    Feature featureToBind, 
                    QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#getContinuityLine()
   */
  public ILineElement getContinuityLine( )
  {
    ILineElement resolvedLink = FeatureHelper.resolveLink( 
                    this, 
                    Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE, 
                    ILineElement.class );
    return resolvedLink;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#getElement1D()
   */
  public IElement1D getElement1D( )
  {
    IElement1D resolvedLink = FeatureHelper.resolveLink( 
        this, 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT1D,
        IElement1D.class );
    return resolvedLink;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement#addElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  public boolean addElementAsRef( IFE1D2DElement element )
  {
    Feature wrappedFeature = element.getWrappedFeature();
    if(TypeInfo.isBoundaryLine( wrappedFeature ))
    {
      getFeature().setProperty( 
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE, 
          wrappedFeature.getId() );
      element.getContainers().addRef( this );
      return true;
    }
    else if(TypeInfo.isElement1DFeature( wrappedFeature ))
    {
      getFeature().setProperty( 
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT1D, 
          wrappedFeature.getId() );
      element.getContainers().addRef( this );
      return true;
    }
    else
    {
      String message = 
        String.format( 
            "This complex element accept only one 1D element and one "+
              " boundary line; so adding this feature is not possible"+
              "\n\tfeature to add as ref=%s \n\ttype=", 
            wrappedFeature, 
            wrappedFeature.getFeatureType().getQName() );
     throw new IllegalArgumentException(message); 
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement#getElements()
   */
  public IFeatureWrapperCollection getElements( )
  {
    return (IFeatureWrapperCollection) getJunctionAsList(  );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement#removeElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  public boolean removeElementAsRef( IFE1D2DElement element )
  {
    Feature featureToDel = element.getWrappedFeature();
    if(TypeInfo.isBoundaryLine( featureToDel ))
    {
      if(featureToDel.equals( getContinuityLine().getWrappedFeature() ))
      {
        element.getContainers().removeAllRefs( this );
        getFeature().setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE, 
            null );
        return true;
      }
      else
      {
        return false;
      }
    }
    else if(TypeInfo.isElement1DFeature( featureToDel ))
    {
      if(featureToDel.equals( getElement1D().getWrappedFeature() ))
      {
        element.getContainers().removeAllRefs( this );
        getFeature().setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT1D, 
            null );
        return true;
      }
      else
      {
        return false;
      }
    }
    else
    {
      String message = 
        String.format( 
            "This complex element accept only one 1D element and one "+
              " continuity line; so adding this feature is not possible"+
              "\n\tfeature to add as ref=%s \n\ttype=", 
            featureToDel, 
            featureToDel.getFeatureType().getQName() );
     throw new IllegalArgumentException(message); 
    }
  }
  
  protected List<IFE1D2DElement> collectJunctionElements()
  {
    List<IFE1D2DElement> eleList = new ArrayList<IFE1D2DElement>();
    IElement1D element1D = getElement1D();
    if(element1D != null)
    {
      eleList.add( element1D );
    }
    
    ILineElement continuityLine = getContinuityLine();
    if(continuityLine != null)
    {
      eleList.add( continuityLine );
    }
    return eleList;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#recalculateElementGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    ILineElement<IFE1D2DComplexElement, IFE1D2DEdge> continuityLine = getContinuityLine();
    if(continuityLine == null )
    {
      return null;
    }
    
    IElement1D element1D = getElement1D();
    if(element1D == null )
    {
      return null;
    }
    
    final IFE1D2DNode endNode1D = EdgeOps.find1DEdgeEndNode( element1D.getEdge());
    
    List<IFE1D2DNode> nodes = continuityLine.getNodes();
    nodes.add(0, endNode1D );
    nodes.add( endNode1D );//close exterior ring
    
    return ModelGeometryBuilder.createSurfaceFromNode( nodes );
  }
  
  private final List<IFE1D2DElement> getJunctionAsList()
  {
//    List<IFeatureWrapper2> list = Collections.unmodifiableList( eleList );
    IFeatureWrapperCollection<IFE1D2DElement> list = 
      new IFeatureWrapperCollection<IFE1D2DElement>()
    {

      public boolean add( IFE1D2DElement o )
      {
        return addElementAsRef( o );
      }

      public void add( int index, IFE1D2DElement element )
      {
        throw new UnsupportedOperationException(); 
      }

      public boolean addAll( Collection< ? extends IFE1D2DElement> c )
      {
        throw new UnsupportedOperationException(); 
      }

      public boolean addAll( int index, Collection< ? extends IFE1D2DElement> c )
      {
        throw new UnsupportedOperationException();
      }

      public void clear( )
      {
        Feature contextFeature = getFeature();
        contextFeature.setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE, 
            null );
        contextFeature.setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT1D, 
            null );
      }

      public boolean contains( Object o )
      {
        throw new UnsupportedOperationException();
      }

      public boolean containsAll( Collection< ? > c )
      {
        throw new UnsupportedOperationException();
      }

      public IFE1D2DElement get( int index )
      {
        throw new UnsupportedOperationException();
      }

      public int indexOf( Object o )
      {
        throw new UnsupportedOperationException();
      }

      public boolean isEmpty( )
      {
        return getElement1D()==null  && getContinuityLine()==null;
      }

      public Iterator<IFE1D2DElement> iterator( )
      {
        return collectJunctionElements().iterator();
      }

      public int lastIndexOf( Object o )
      {
        throw new UnsupportedOperationException();
      }

      public ListIterator<IFE1D2DElement> listIterator( )
      {
        throw new UnsupportedOperationException();
      }

      public ListIterator<IFE1D2DElement> listIterator( int index )
      {
        throw new UnsupportedOperationException();
      }

      public boolean remove( Object o )
      {
        if(o instanceof IFE1D2DElement)
        {
         return removeElementAsRef( (IFE1D2DElement) o ); 
        }
        else
        {
          return false;
        }
      }

      public IFE1D2DElement remove( int index )
      {
        throw new UnsupportedOperationException();
      }

      public boolean removeAll( Collection< ? > c )
      {
        throw new UnsupportedOperationException();
      }

      public boolean retainAll( Collection< ? > c )
      {
        throw new UnsupportedOperationException();
      }

      public IFE1D2DElement set( int index, IFE1D2DElement element )
      {
        throw new UnsupportedOperationException();
      }

      public int size( )
      {
        return collectJunctionElements().size();
      }

      public List<IFE1D2DElement> subList( int fromIndex, int toIndex )
      {
        throw new UnsupportedOperationException();
      }

      public Object[] toArray( )
      {
        return null;
      }

      public <T> T[] toArray( T[] a )
      {
        return null;
      }

      public IFE1D2DElement addNew( QName newChildType )
      {
        return null;
      }

      public <T extends IFE1D2DElement> T addNew( QName newChildType, Class<T> classToAdapt )
      {
        return null;
      }

      public IFE1D2DElement addNew( QName newChildType, String newFeatureId )
      {
        return null;
      }

      public <T extends IFE1D2DElement> T addNew( QName newChildType, String newFeatureId, Class<T> classToAdapt )
      {
        return null;
      }

      public IFE1D2DElement addNew( int index, QName newChildType )
      {
        return null;
      }

      public <T extends IFE1D2DElement> T addNew( int index, QName newChildType, Class<T> classToAdapt )
      {
        return null;
      }

      public boolean addRef( IFE1D2DElement toAdd ) throws IllegalArgumentException
      {
        return false;
      }

      public FeatureList getWrappedList( )
      {
        return null;
      }

      public boolean removeAllRefs( IFE1D2DElement toRemove ) throws IllegalArgumentException
      {
        return false;
      }

      public String getDescription( )
      {
        return null;
      }

      public String getGmlID( )
      {
        return null;
      }

      public String getName( )
      {
        return null;
      }

      public Feature getWrappedFeature( )
      {
        return null;
      }

      public void setDescription( String desc )
      {
        
      }

      public void setName( String name )
      {
        
      }

      public List<IFE1D2DElement> query( GM_Surface selectionSurface, boolean containedOnly, QName checkedGeometryPropertyName )
      {
        return null;
      }

      public List<IFE1D2DElement> query( GM_Envelope envelope )
      {
        return null;
      }

      public List<IFE1D2DElement> query( GM_Position position )
      {
        return null;
      }

      public int countFeatureWrappers( Class wrapperClass )
      {
        return 0;
      }
      
    };
    
    return list;
  }
  
  
}
