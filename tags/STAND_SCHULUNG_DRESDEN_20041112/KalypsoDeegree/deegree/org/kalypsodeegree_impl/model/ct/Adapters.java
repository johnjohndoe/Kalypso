/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
 (C) 2001, Institut de Recherche pour le D�veloppement (http://sourceforge.net/projects/seagis/)
 SEAGIS Contacts:  Surveillance de l'Environnement Assist�e par Satellite
 Institut de Recherche pour le D�veloppement / US-Espace
 mailto:seasnet@teledetection.fr


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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.ct;

// OpenGIS dependencies
import java.rmi.RemoteException;

import javax.media.jai.ParameterList;
import javax.media.jai.ParameterListDescriptorImpl;
import javax.media.jai.ParameterListImpl;

import org.deegree_impl.model.resources.WeakHashSet;
import org.deegree_impl.model.resources.XArray;
import org.opengis.ct.CT_CoordinateTransformation;
import org.opengis.ct.CT_CoordinateTransformationFactory;
import org.opengis.ct.CT_DomainFlags;
import org.opengis.ct.CT_MathTransform;
import org.opengis.ct.CT_MathTransformFactory;
import org.opengis.ct.CT_Parameter;
import org.opengis.ct.CT_TransformType;

/**
 * <FONT COLOR="#FF6633">Provide methods for interoperability with
 * <code>org.opengis.ct</code> package. </FONT> All methods accept null
 * argument. All OpenGIS objects are suitable for RMI use.
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
   * The underlying adapters from the <code>org.deegree_impl.model.cs</code>
   * package.
   */
  public final org.deegree_impl.model.cs.Adapters CS;

  /**
   * The underlying adapters from the <code>org.deegree_impl.model.pt</code>
   * package.
   */
  public final org.deegree_impl.model.pt.Adapters PT;

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
      return super.equals( MathTransformExport.unwrap( object1 ), MathTransformExport
          .unwrap( object2 ) );
    }
  };

  /**
   * Default constructor.
   * 
   * @param CS
   *          The underlying adapters from the
   *          <code>org.deegree_impl.model.cs</code> package.
   */
  protected Adapters( final org.deegree_impl.model.cs.Adapters CS )
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
      DEFAULT = new Adapters( org.deegree_impl.model.cs.Adapters.getDefault() );
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
    return ( transform != null ) ? (CT_CoordinateTransformation)transform.cachedOpenGIS( this )
        : null;
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
    return ( factory != null ) ? (CT_CoordinateTransformationFactory)factory.toOpenGIS( this )
        : null;
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
    else
      return new MathTransformAdapter( transform );
  }

  /**
   * Returns a coordinate transform for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public CoordinateTransformation wrap( final CT_CoordinateTransformation transform )
      throws RemoteException
  {
    if( transform == null )
      return null;
    if( transform instanceof CoordinateTransformation.Export )
    {
      return ( (CoordinateTransformation.Export)transform ).unwrap();
    }
    return new CoordinateTransformation( null, CS.wrap( transform.getSourceCS() ), CS
        .wrap( transform.getTargetCS() ), wrap( transform.getTransformType() ), wrap( transform
        .getMathTransform() ) );
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
    final ParameterList list = new ParameterListImpl( new ParameterListDescriptorImpl( null,
        paramNames, paramClasses, null, null ) );
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