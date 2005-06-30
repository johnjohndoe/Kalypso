/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.ct;

// OpenGIS dependencies
import java.rmi.RemoteException;

import javax.media.jai.ParameterList;
import javax.media.jai.ParameterListDescriptorImpl;
import javax.media.jai.ParameterListImpl;

import org.kalypsodeegree_impl.model.resources.WeakHashSet;
import org.kalypsodeegree_impl.model.resources.XArray;
import org.opengis.ct.CT_CoordinateTransformation;
import org.opengis.ct.CT_CoordinateTransformationFactory;
import org.opengis.ct.CT_DomainFlags;
import org.opengis.ct.CT_MathTransform;
import org.opengis.ct.CT_MathTransformFactory;
import org.opengis.ct.CT_Parameter;
import org.opengis.ct.CT_TransformType;

/**
 * <FONT COLOR="#FF6633">Provide methods for interoperability with <code>org.opengis.ct</code> package. </FONT> All
 * methods accept null argument. All OpenGIS objects are suitable for RMI use.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class Adapters
{
  /**
   * Default adapters. Will be constructed only when first requested.
   */
  private static Adapters DEFAULT;

  /**
   * The underlying adapters from the <code>org.kalypsodeegree_impl.model.cs</code> package.
   */
  public final org.kalypsodeegree_impl.model.cs.Adapters CS;

  /**
   * The underlying adapters from the <code>org.kalypsodeegree_impl.model.pt</code> package.
   */
  public final org.kalypsodeegree_impl.model.pt.Adapters PT;

  /**
   * A weak hash set for for {@link MathTransformExport}objects.
   */
  private static final WeakHashSet MATH_EXPORTS = new WeakHashSet()
  {
    protected int hashCode( final Object object )
    {
      return super.hashCode( MathTransformExport.unwrap( object ) );
    }

    protected boolean equals( final Object object1, final Object object2 )
    {
      return super.equals( MathTransformExport.unwrap( object1 ), MathTransformExport.unwrap( object2 ) );
    }
  };

  /**
   * Default constructor.
   * 
   * @param CS
   *          The underlying adapters from the <code>org.kalypsodeegree_impl.model.cs</code> package.
   */
  protected Adapters( final org.kalypsodeegree_impl.model.cs.Adapters CS )
  {
    this.CS = CS;
    this.PT = CS.PT;
  }

  /**
   * Gets the default adapters.
   */
  public static synchronized Adapters getDefault()
  {
    if( DEFAULT == null )
    {
      DEFAULT = new Adapters( org.kalypsodeegree_impl.model.cs.Adapters.getDefault() );
    }
    return DEFAULT;
  }

  /**
   * Returns an OpenGIS interface for a math transform.
   */
  public CT_MathTransform export( final MathTransform transform )
  {
    if( transform == null )
      return null;
    Object exported = MATH_EXPORTS.get( transform );
    if( exported == null )
    {
      if( transform instanceof AbstractMathTransform )
        exported = ( (AbstractMathTransform)transform ).toOpenGIS( this );
      else
        exported = new MathTransformExport( this, transform );
      exported = MATH_EXPORTS.intern( exported );
    }
    return (CT_MathTransform)exported;
  }

  /**
   * Returns an OpenGIS interface for a math transform.
   */
  public CT_CoordinateTransformation export( final CoordinateTransformation transform )
  {
    return ( transform != null ) ? (CT_CoordinateTransformation)transform.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a math transform factory.
   */
  public CT_MathTransformFactory export( final MathTransformFactory factory )
  {
    return ( factory != null ) ? (CT_MathTransformFactory)factory.toOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a coordinate transformation factory.
   */
  public CT_CoordinateTransformationFactory export( final CoordinateTransformationFactory factory )
  {
    return ( factory != null ) ? (CT_CoordinateTransformationFactory)factory.toOpenGIS( this ) : null;
  }

  /**
   * Construct an array of OpenGIS structure from a parameter list.
   */
  public CT_Parameter[] export( final ParameterList parameters )
  {
    if( parameters == null )
      return null;
    final String[] names = parameters.getParameterListDescriptor().getParamNames();
    final CT_Parameter[] param = new CT_Parameter[names != null ? names.length : 0];
    int count = 0;
    for( int i = 0; i < param.length; i++ )
    {
      final String name = names[i];
      final Object value;
      try
      {
        value = parameters.getObjectParameter( name );
      }
      catch( IllegalStateException exception )
      {
        // No value and no default. Ignore...
        continue;
      }
      if( value instanceof Number )
      {
        param[count++] = new CT_Parameter( name, ( (Number)value ).doubleValue() );
      }
    }
    return (CT_Parameter[])XArray.resize( param, count );
  }

  /**
   * Construct an OpenGIS enum from a transform type.
   */
  public CT_TransformType export( final TransformType type )
  {
    return ( type != null ) ? new CT_TransformType( type.getValue() ) : null;
  }

  /**
   * Construct an OpenGIS enum from a domain flag.
   */
  public CT_DomainFlags export( final DomainFlags flags )
  {
    return ( flags != null ) ? new CT_DomainFlags( flags.getValue() ) : null;
  }

  /**
   * Returns a math transform for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public MathTransform wrap( final CT_MathTransform transform ) throws RemoteException
  {
    if( transform == null )
      return null;
    if( transform instanceof MathTransformExport )
    {
      return ( (MathTransformExport)transform ).transform;
    }
    if( transform.getDimSource() == 2 && transform.getDimTarget() == 2 )
    {
      return new MathTransformAdapter2D( transform );
    }

    return new MathTransformAdapter( transform );
  }

  /**
   * Returns a coordinate transform for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public CoordinateTransformation wrap( final CT_CoordinateTransformation transform ) throws RemoteException
  {
    if( transform == null )
      return null;
    if( transform instanceof CoordinateTransformation.Export )
    {
      return ( (CoordinateTransformation.Export)transform ).unwrap();
    }
    return new CoordinateTransformation( null, CS.wrap( transform.getSourceCS() ), CS.wrap( transform.getTargetCS() ),
        wrap( transform.getTransformType() ), wrap( transform.getMathTransform() ) );
  }

  /**
   * Returns a parameter list for an array of OpenGIS structure.
   */
  public ParameterList wrap( final CT_Parameter[] parameters )
  {
    if( parameters == null )
      return null;
    int count = 0;
    String[] paramNames = new String[parameters.length];
    Class[] paramClasses = new Class[parameters.length];
    for( int i = 0; i < parameters.length; i++ )
    {
      final CT_Parameter param = parameters[i];
      if( param != null )
      {
        paramNames[count] = param.name;
        paramClasses[count] = Double.class;
        count++;
      }
    }
    paramNames = (String[])XArray.resize( paramNames, count );
    paramClasses = (Class[])XArray.resize( paramClasses, count );
    final ParameterList list = new ParameterListImpl( new ParameterListDescriptorImpl( null, paramNames, paramClasses,
        null, null ) );
    for( int i = 0; i < paramNames.length; i++ )
    {
      list.setParameter( paramNames[i], parameters[i].value );
    }
    return list;
  }

  /**
   * Construct a transform type from an OpenGIS enum.
   */
  public TransformType wrap( final CT_TransformType type )
  {
    return ( type != null ) ? TransformType.getEnum( type.value ) : null;
  }

  /**
   * Construct a domain flag from an OpenGIS enum.
   */
  public DomainFlags wrap( final CT_DomainFlags flags )
  {
    return ( flags != null ) ? DomainFlags.getEnum( flags.value ) : null;
  }
}