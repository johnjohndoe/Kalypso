/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
 (C) 2001, Institut de Recherche pour le Développement (http://sourceforge.net/projects/seagis/)
 SEAGIS Contacts:  Surveillance de l'Environnement Assistée par Satellite
 Institut de Recherche pour le Développement / US-Espace
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
package org.deegree_impl.model.cs;

// OpenGIS dependencies
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;

import javax.media.jai.ParameterList;
import javax.media.jai.ParameterListDescriptorImpl;
import javax.media.jai.ParameterListImpl;
import javax.units.Unit;

import org.deegree_impl.model.cs.Info.AngularUnit;
import org.deegree_impl.model.cs.Info.LinearUnit;
import org.deegree_impl.model.resources.XArray;
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;
import org.opengis.cs.CS_AngularUnit;
import org.opengis.cs.CS_AxisInfo;
import org.opengis.cs.CS_AxisOrientationEnum;
import org.opengis.cs.CS_CompoundCoordinateSystem;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.cs.CS_CoordinateSystemFactory;
import org.opengis.cs.CS_Datum;
import org.opengis.cs.CS_DatumType;
import org.opengis.cs.CS_Ellipsoid;
import org.opengis.cs.CS_GeocentricCoordinateSystem;
import org.opengis.cs.CS_GeographicCoordinateSystem;
import org.opengis.cs.CS_HorizontalCoordinateSystem;
import org.opengis.cs.CS_HorizontalDatum;
import org.opengis.cs.CS_Info;
import org.opengis.cs.CS_LinearUnit;
import org.opengis.cs.CS_LocalCoordinateSystem;
import org.opengis.cs.CS_LocalDatum;
import org.opengis.cs.CS_PrimeMeridian;
import org.opengis.cs.CS_ProjectedCoordinateSystem;
import org.opengis.cs.CS_Projection;
import org.opengis.cs.CS_ProjectionParameter;
import org.opengis.cs.CS_Unit;
import org.opengis.cs.CS_VerticalCoordinateSystem;
import org.opengis.cs.CS_VerticalDatum;
import org.opengis.cs.CS_WGS84ConversionInfo;

/**
 * <FONT COLOR="#FF6633">Provide methods for interoperability with
 * <code>org.opengis.cs</code> package. </FONT> All methods accept null
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
   * The underlying adapters for the <code>org.deegree_impl.model.pt</code>
   * package.
   */
  public org.deegree_impl.model.pt.Adapters PT = null;

  protected Adapters()
  {}

  /**
   * Default constructor.
   * 
   * @param PT
   *          The underlying adapters for <code>org.deegree_impl.model.pt</code>
   *          package.
   */
  protected Adapters( final org.deegree_impl.model.pt.Adapters PT )
  {
    this.PT = PT;
  }

  /**
   * Gets the default adapters.
   */
  public static synchronized Adapters getDefault()
  {
    if( DEFAULT == null )
    {
      DEFAULT = new Adapters( org.deegree_impl.model.pt.Adapters.getDefault() );
    }
    return DEFAULT;
  }

  /**
   * Returns an OpenGIS interface for a coordinate system factory.
   */
  public CS_CoordinateSystemFactory export( final CoordinateSystemFactory factory )
  {
    return ( factory != null ) ? (CS_CoordinateSystemFactory)factory.toOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for an info. If the argument is an
   * <code>Info</code> subclass, the returned object will implements the
   * corresponding interface. For example a call with an argument of type
   * {@link GeographicCoordinateSystem}will returns an object implementing the
   * {@link CS_GeographicCoordinateSystem}interface.
   */
  public CS_Info export( final Info info )
  {
    return ( info != null ) ? (CS_Info)info.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a coordinate system. If the argument is a
   * <code>CoordinateSystem</code> subclass, the returned object will
   * implements the corresponding interface.
   */
  public CS_CoordinateSystem export( final CoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_CoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a compound coordinate system.
   */
  public CS_CompoundCoordinateSystem export( final CompoundCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_CompoundCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a local coordinate system.
   */
  public CS_LocalCoordinateSystem export( final LocalCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_LocalCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a geocentric coordinate system.
   */
  public CS_GeocentricCoordinateSystem export( final GeocentricCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_GeocentricCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a vertical coordinate system.
   */
  public CS_VerticalCoordinateSystem export( final VerticalCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_VerticalCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a horizontal coordinate system.
   */
  public CS_HorizontalCoordinateSystem export( final HorizontalCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_HorizontalCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a geographic coordinate system.
   */
  public CS_GeographicCoordinateSystem export( final GeographicCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_GeographicCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a projected coordinate system.
   */
  public CS_ProjectedCoordinateSystem export( final ProjectedCoordinateSystem cs )
  {
    return ( cs != null ) ? (CS_ProjectedCoordinateSystem)cs.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a projection.
   */
  public CS_Projection export( final Projection projection )
  {
    return ( projection != null ) ? (CS_Projection)projection.cachedOpenGIS( this ) : null;
  }

  /**
   * Construct an array of OpenGIS structure from a parameters list.
   */
  public CS_ProjectionParameter[] export( final ParameterList parameters )
  {
    if( parameters == null )
      return null;
    final String[] names = parameters.getParameterListDescriptor().getParamNames();
    final CS_ProjectionParameter[] param = new CS_ProjectionParameter[names != null ? names.length
        : 0];
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
        param[count++] = new CS_ProjectionParameter( name, ( (Number)value ).doubleValue() );
      }
    }
    return (CS_ProjectionParameter[])XArray.resize( param, count );
  }

  /**
   * Returns an OpenGIS interface for a prime meridien.
   */
  public CS_PrimeMeridian export( final PrimeMeridian meridian )
  {
    return ( meridian != null ) ? (CS_PrimeMeridian)meridian.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for an ellipsoid.
   */
  public CS_Ellipsoid export( final Ellipsoid ellipsoid )
  {
    return ( ellipsoid != null ) ? (CS_Ellipsoid)ellipsoid.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS enumeration for a datum type.
   */
  public CS_DatumType export( final DatumType type )
  {
    return ( type != null ) ? new CS_DatumType( type.getValue() ) : null;
  }

  /**
   * Returns an OpenGIS interface for a datum.
   */
  public CS_Datum export( final Datum datum )
  {
    return ( datum != null ) ? (CS_Datum)datum.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a datum.
   */
  public CS_LocalDatum export( final LocalDatum datum )
  {
    return ( datum != null ) ? (CS_LocalDatum)datum.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a datum.
   */
  public CS_HorizontalDatum export( final HorizontalDatum datum )
  {
    return ( datum != null ) ? (CS_HorizontalDatum)datum.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS interface for a datum.
   */
  public CS_VerticalDatum export( final VerticalDatum datum )
  {
    return ( datum != null ) ? (CS_VerticalDatum)datum.cachedOpenGIS( this ) : null;
  }

  /**
   * Returns an OpenGIS enumeration for an axis orientation.
   */
  public CS_AxisOrientationEnum export( final AxisOrientation orientation )
  {
    return ( orientation != null ) ? new CS_AxisOrientationEnum( orientation.getValue() ) : null;
  }

  /**
   * Returns an OpenGIS structure for an axis info.
   */
  public CS_AxisInfo export( final AxisInfo axis )
  {
    return ( axis != null ) ? new CS_AxisInfo( axis.name, export( axis.orientation ) ) : null;
  }

  /**
   * Returns an OpenGIS structure for conversion info.
   */
  public CS_WGS84ConversionInfo export( final WGS84ConversionInfo info )
  {
    if( info == null )
      return null;
    final CS_WGS84ConversionInfo nf = new CS_WGS84ConversionInfo();
    nf.dx = info.dx;
    nf.dy = info.dy;
    nf.dz = info.dz;
    nf.ex = info.ex;
    nf.ey = info.ey;
    nf.ez = info.ez;
    nf.ppm = info.ppm;
    nf.areaOfUse = info.areaOfUse;
    return nf;
  }

  /**
   * Returns an OpenGIS interface for an unit. The returned interface may
   * extends {@link CS_LinearUnit}or {@link CS_AngularUnit}according the
   * specified unit.
   */
  public CS_Unit export( final Unit unit )
  {
    if( unit == null )
      return null;
    if( unit.canConvert( Unit.METRE ) )
    {
      return new Info( unit.toString() ).new LinearUnit( this, unit.convert( 1, Unit.METRE ) );
    }
    if( unit.canConvert( Unit.DEGREE ) )
    {
      return new Info( unit.toString() ).new AngularUnit( this, Math.toRadians( unit.convert( 1,
          Unit.DEGREE ) ) );
    }
    throw new UnsupportedOperationException(
        "Only linear and angular units are currently implemented" );
  }

  /**
   * Check if the specified coordinate system has the expected number of
   * dimensions.
   * 
   * @param cs
   *          The coordinate system to check.
   * @param expected
   *          The expected number of dimensions.
   * @throws IllegalArgumentException
   *           if the coordinate system doesn't have the expected number of
   *           dimensions.
   */
  private static void checkDimension( final CS_CoordinateSystem cs, final int expected )
      throws RemoteException, IllegalArgumentException
  {
    final int dimension = cs.getDimension();
    if( dimension != expected )
    {
      throw new IllegalArgumentException( Resources.format(
          ResourceKeys.ERROR_ILLEGAL_CS_DIMENSION_$1, new Integer( dimension ) ) );
    }
  }

  /**
   * Returns info for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public Info wrap( final CS_Info info ) throws RemoteException
  {
    if( info == null )
      return null;
    if( info instanceof CS_Datum )
      return wrap( (CS_Datum)info );
    if( info instanceof CS_CoordinateSystem )
      return wrap( (CS_CoordinateSystem)info );
    if( info instanceof Info.Export )
    {
      return ( (Info.Export)info ).unwrap();
    }
    return new Info( map( info ) );
  }

  /**
   * Returns a coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public CoordinateSystem wrap( final CS_CoordinateSystem cs ) throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CS_CompoundCoordinateSystem )
      return wrap( (CS_CompoundCoordinateSystem)cs );
    if( cs instanceof CS_LocalCoordinateSystem )
      return wrap( (CS_LocalCoordinateSystem)cs );
    if( cs instanceof CS_GeocentricCoordinateSystem )
      return wrap( (CS_GeocentricCoordinateSystem)cs );
    if( cs instanceof CS_VerticalCoordinateSystem )
      return wrap( (CS_VerticalCoordinateSystem)cs );
    if( cs instanceof CS_HorizontalCoordinateSystem )
      return wrap( (CS_HorizontalCoordinateSystem)cs );
    if( cs instanceof CoordinateSystem.Export )
    {
      return (CoordinateSystem)( (CoordinateSystem.Export)cs ).unwrap();
    }
    throw new UnsupportedOperationException( "Unknow CS not yet implemented" ); // CoordinateSystem
    // is
    // abstract
  }

  /**
   * Returns a compound coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public CompoundCoordinateSystem wrap( final CS_CompoundCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CoordinateSystem.Export )
    {
      return (CompoundCoordinateSystem)( (CoordinateSystem.Export)cs ).unwrap();
    }
    return new CompoundCoordinateSystem( map( cs ), wrap( cs.getHeadCS() ), wrap( cs.getTailCS() ) );
  }

  /**
   * Returns a local coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public LocalCoordinateSystem wrap( final CS_LocalCoordinateSystem cs ) throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CoordinateSystem.Export )
    {
      return (LocalCoordinateSystem)( (CoordinateSystem.Export)cs ).unwrap();
    }
    final LocalDatum datum = wrap( cs.getLocalDatum() );
    final Unit[] unit = new Unit[cs.getDimension()];
    final AxisInfo[] axes = new AxisInfo[unit.length];
    for( int i = 0; i < axes.length; i++ )
    {
      unit[i] = wrap( cs.getUnits( i ) );
      axes[i] = wrap( cs.getAxis( i ) );
      // Accept null value.
    }
    return new LocalCoordinateSystem( map( cs ), datum, unit, axes );
  }

  /**
   * Returns a geocentric coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public GeocentricCoordinateSystem wrap( final CS_GeocentricCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CoordinateSystem.Export )
    {
      return (GeocentricCoordinateSystem)( (CoordinateSystem.Export)cs ).unwrap();
    }
    checkDimension( cs, 3 );
    final Unit unit = wrap( cs.getLinearUnit() );
    final HorizontalDatum datum = wrap( cs.getHorizontalDatum() );
    final PrimeMeridian meridian = wrap( cs.getPrimeMeridian() );
    final AxisInfo[] axes = new AxisInfo[cs.getDimension()];
    for( int i = 0; i < axes.length; i++ )
    {
      axes[i] = wrap( cs.getAxis( i ) );
      // Accept null value.
    }
    return new GeocentricCoordinateSystem( map( cs ), unit, datum, meridian, axes );
  }

  /**
   * Returns a vertical coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public VerticalCoordinateSystem wrap( final CS_VerticalCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CoordinateSystem.Export )
    {
      return (VerticalCoordinateSystem)( (CoordinateSystem.Export)cs ).unwrap();
    }
    checkDimension( cs, 1 );
    final VerticalDatum datum = wrap( cs.getVerticalDatum() );
    final Unit unit = wrap( cs.getVerticalUnit() );
    final AxisInfo axis = wrap( cs.getAxis( 0 ) );
    return new VerticalCoordinateSystem( map( cs ), datum, unit, axis );
  }

  /**
   * Returns a horizontal coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public HorizontalCoordinateSystem wrap( final CS_HorizontalCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof CS_GeographicCoordinateSystem )
      return wrap( (CS_GeographicCoordinateSystem)cs );
    if( cs instanceof CS_ProjectedCoordinateSystem )
      return wrap( (CS_ProjectedCoordinateSystem)cs );
    if( cs instanceof HorizontalCoordinateSystem.Export )
    {
      return (HorizontalCoordinateSystem)( (HorizontalCoordinateSystem.Export)cs ).unwrap();
    }
    throw new UnsupportedOperationException( "Unknow CS not yet implemented" ); // HorizontalCoordinateSystem
    // is
    // abstract
  }

  /**
   * Returns a geographic coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public GeographicCoordinateSystem wrap( final CS_GeographicCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof HorizontalCoordinateSystem.Export )
    {
      return (GeographicCoordinateSystem)( (HorizontalCoordinateSystem.Export)cs ).unwrap();
    }
    checkDimension( cs, 2 );
    final Unit unit = wrap( cs.getAngularUnit() );
    final HorizontalDatum datum = wrap( cs.getHorizontalDatum() );
    final PrimeMeridian meridian = wrap( cs.getPrimeMeridian() );
    final AxisInfo axis0 = wrap( cs.getAxis( 0 ) );
    final AxisInfo axis1 = wrap( cs.getAxis( 1 ) );
    return new GeographicCoordinateSystem( map( cs ), unit, datum, meridian, axis0, axis1 );
  }

  /**
   * Returns a projected coordinate system for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public ProjectedCoordinateSystem wrap( final CS_ProjectedCoordinateSystem cs )
      throws RemoteException
  {
    if( cs == null )
      return null;
    if( cs instanceof HorizontalCoordinateSystem.Export )
    {
      return (ProjectedCoordinateSystem)( (HorizontalCoordinateSystem.Export)cs ).unwrap();
    }
    checkDimension( cs, 2 );
    final GeographicCoordinateSystem gcs = wrap( cs.getGeographicCoordinateSystem() );
    final Projection projection = wrap( cs.getProjection() );
    final Unit unit = wrap( cs.getLinearUnit() );
    final AxisInfo axis0 = wrap( cs.getAxis( 0 ) );
    final AxisInfo axis1 = wrap( cs.getAxis( 1 ) );
    return new ProjectedCoordinateSystem( map( cs ), gcs, projection, unit, axis0, axis1 );
  }

  /**
   * Returns a projection for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public Projection wrap( final CS_Projection projection ) throws RemoteException
  {
    if( projection == null )
      return null;
    if( projection instanceof Info.Export )
    {
      return (Projection)( (Info.Export)projection ).unwrap();
    }
    final CS_ProjectionParameter[] parameters = new CS_ProjectionParameter[projection
        .getNumParameters()];
    for( int i = 0; i < parameters.length; i++ )
    {
      parameters[i] = projection.getParameter( i );
    }
    return new Projection( map( projection ), projection.getClassName(), wrap( parameters ) );
  }

  /**
   * Returns a prime meridian for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public PrimeMeridian wrap( final CS_PrimeMeridian meridian ) throws RemoteException
  {
    if( meridian == null )
      return null;
    if( meridian instanceof Info.Export )
    {
      return (PrimeMeridian)( (Info.Export)meridian ).unwrap();
    }
    return new PrimeMeridian( map( meridian ), wrap( meridian.getAngularUnit() ), meridian
        .getLongitude() );
  }

  /**
   * Returns an ellipsoid for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public Ellipsoid wrap( final CS_Ellipsoid ellipsoid ) throws RemoteException
  {
    if( ellipsoid == null )
      return null;
    if( ellipsoid instanceof Info.Export )
    {
      return (Ellipsoid)( (Info.Export)ellipsoid ).unwrap();
    }
    final double ivf = ellipsoid.getInverseFlattening();
    return new Ellipsoid( map( ellipsoid ), ellipsoid.getSemiMajorAxis(), ellipsoid
        .getSemiMinorAxis(), ivf != 0 ? ivf : Double.POSITIVE_INFINITY,
        ellipsoid.isIvfDefinitive(), wrap( ellipsoid.getAxisUnit() ) );
  }

  /**
   * Returns a datum type for an OpenGIS enumeration.
   */
  public DatumType wrap( final CS_DatumType type )
  {
    return ( type != null ) ? DatumType.getEnum( type.value ) : null;
  }

  /**
   * Returns a datum for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public Datum wrap( final CS_Datum datum ) throws RemoteException
  {
    if( datum == null )
      return null;
    if( datum instanceof CS_LocalDatum )
      return wrap( (CS_LocalDatum)datum );
    if( datum instanceof CS_VerticalDatum )
      return wrap( (CS_VerticalDatum)datum );
    if( datum instanceof CS_HorizontalDatum )
      return wrap( (CS_HorizontalDatum)datum );
    if( datum instanceof Datum.Export )
    {
      return (Datum)( (Datum.Export)datum ).unwrap();
    }
    return new Datum( map( datum ), wrap( datum.getDatumType() ) );
  }

  /**
   * Returns a local datum for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public LocalDatum wrap( final CS_LocalDatum datum ) throws RemoteException
  {
    if( datum == null )
      return null;
    if( datum instanceof Datum.Export )
    {
      return (LocalDatum)( (Datum.Export)datum ).unwrap();
    }
    return new LocalDatum( map( datum ), wrap( datum.getDatumType() ) );
  }

  /**
   * Returns a horizontal datum for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public HorizontalDatum wrap( final CS_HorizontalDatum datum ) throws RemoteException
  {
    if( datum == null )
      return null;
    if( datum instanceof Datum.Export )
    {
      return (HorizontalDatum)( (Datum.Export)datum ).unwrap();
    }
    return new HorizontalDatum( map( datum ), wrap( datum.getDatumType() ), wrap( datum
        .getEllipsoid() ), wrap( datum.getWGS84Parameters() ) );
  }

  /**
   * Returns a vertical datum for an OpenGIS interface.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public VerticalDatum wrap( final CS_VerticalDatum datum ) throws RemoteException
  {
    if( datum == null )
      return null;
    if( datum instanceof Datum.Export )
    {
      return (VerticalDatum)( (Datum.Export)datum ).unwrap();
    }
    return new VerticalDatum( map( datum ), wrap( datum.getDatumType() ) );
  }

  /**
   * Returns an axis orientation for an OpenGIS enumeration.
   */
  public AxisOrientation wrap( final CS_AxisOrientationEnum orientation )
  {
    return ( orientation != null ) ? AxisOrientation.getEnum( orientation.value ) : null;
  }

  /**
   * Returns an axis info for an OpenGIS structure.
   */
  public AxisInfo wrap( final CS_AxisInfo axis )
  {
    return ( axis != null ) ? new AxisInfo( axis.name, wrap( axis.orientation ) ) : null;
  }

  /**
   * Returns an axis array for an OpenGIS structure array.
   */
  final AxisInfo[] wrap( final CS_AxisInfo[] axis )
  {
    if( axis == null )
      return null;
    final AxisInfo[] a = new AxisInfo[axis.length];
    for( int i = 0; i < axis.length; i++ )
      a[i] = wrap( axis[i] );
    return a;
  }

  /**
   * Returns a parameter list for an array of OpenGIS structures.
   */
  public ParameterList wrap( final CS_ProjectionParameter[] parameters )
  {
    if( parameters == null )
      return null;
    int count = 0;
    String[] paramNames = new String[parameters.length];
    Class[] paramClasses = new Class[parameters.length];
    for( int i = 0; i < parameters.length; i++ )
    {
      final CS_ProjectionParameter param = parameters[i];
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
   * Returns conversion info for an OpenGIS structure.
   */
  public WGS84ConversionInfo wrap( final CS_WGS84ConversionInfo info )
  {
    if( info == null )
      return null;
    final WGS84ConversionInfo nf = new WGS84ConversionInfo();
    nf.dx = info.dx;
    nf.dy = info.dy;
    nf.dz = info.dz;
    nf.ex = info.ex;
    nf.ey = info.ey;
    nf.ez = info.ez;
    nf.ppm = info.ppm;
    nf.areaOfUse = info.areaOfUse;
    return nf;
  }

  /**
   * Returns an unit for an OpenGIS structure.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public Unit wrap( final CS_Unit unit ) throws RemoteException
  {
    if( unit == null )
      return null;
    if( unit instanceof CS_LinearUnit )
    {
      final double metersPerUnit = ( (CS_LinearUnit)unit ).getMetersPerUnit();
      //return Unit.METRE.scale(metersPerUnit);
      return null;
    }
    if( unit instanceof CS_AngularUnit )
    {
      final double radiansPerUnit = ( (CS_AngularUnit)unit ).getRadiansPerUnit();
      //return Unit.RADIAN.scale(radiansPerUnit);
      return null;
    }
    throw new UnsupportedOperationException( "Only meters and degrees are currently implemented" );
  }

  /**
   * Returns a map for the specified OpenGIS structure. Note: current
   * implementation search all info immediatly. Future implementation may differ
   * fetching until needed.
   * 
   * @param info
   *          The OpenGIS structure.
   * @throws RemoteException
   *           if a remote call failed.
   */
  private static Map map( final CS_Info info ) throws RemoteException
  {
    final Map properties = new HashMap( 16 );
    properties.put( "name", info.getName() );
    properties.put( "authority", info.getAuthority() );
    properties.put( "authorityCode", info.getAuthorityCode() );
    properties.put( "alias", info.getAlias() );
    properties.put( "abbreviation", info.getAbbreviation() );
    properties.put( "remarks", info.getRemarks() );
    properties.put( "WKT", info.getWKT() );
    properties.put( "XML", info.getXML() );
    properties.put( "proxy", info );
    return properties;
  }
}