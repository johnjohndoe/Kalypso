package org.kalypso.kalypsosimulationmodel.core;

import java.io.File;
import java.util.Collection;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides assertion methods
 *
 * @author Patrice Congo
 */
public class Assert
{
  private Assert( )
  {
    // empty
  }

  /**
   * Assert the given object for null value. This method throws concequently an illegal argument exception if the passed
   * object is null
   *
   * @param obj
   *            the object to be asserted
   * @param message
   *            the exception message
   * @throws IllegalArgumentException
   *             if the passed object is null
   */
  public static final void throwIAEOnNull( final Object obj, final String message ) throws IllegalArgumentException
  {
    if( obj == null )
    {
      throw new IllegalArgumentException( message );
    }
  }

  /**
   * Assert the given object for null value. This method throws concequently an illegal argument exception if the passed
   * object is null
   *
   * @param obj
   *            the object to be asserted
   * @param message
   *            the exception message
   * @throws IllegalArgumentException
   *             if the passed object is null
   */
  public static final void throwIAEOnNullParam( final Object param, final String paramName ) throws IllegalArgumentException
  {
    if( param == null )
    {
      final StringBuffer buf = new StringBuffer( 128 );
      buf.append( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.0" ) ); //$NON-NLS-1$
      buf.append( paramName );
      throw new IllegalArgumentException( buf.toString() );
    }
  }

  /**
   * Assert the given object for null value. This method throws concequently an illegal argument exception if the passed
   * object is null
   *
   * @param obj
   *            the object to be asserted
   * @param message
   *            the exception message
   * @throws IllegalArgumentException
   *             if the passed object is null
   */
  public static final void throwIAEOnCollectionNullOrHasNullElements( final Collection param, final String paramName ) throws IllegalArgumentException
  {
    if( param == null )
    {
      final StringBuffer buf = new StringBuffer( 128 );
      buf.append( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.1" ) ); //$NON-NLS-1$
      buf.append( paramName );
      throw new IllegalArgumentException( buf.toString() );
    }

    for( final Object ele : param )
    {
      if( ele == null )
      {
        final String message = Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.2"  + Messages.getString("org.kalypso.kalypsosimulationmodel.core.Assert.3"), paramName, param ); //$NON-NLS-1$ //$NON-NLS-2$
        throw new IllegalArgumentException( message );
      }
    }
  }

  public static final void throwIAEOnCollectionNullOrEmpty( final Collection param, final String paramName ) throws IllegalArgumentException
  {
    if( param == null )
    {
      final String message =  Messages.getString("org.kalypso.kalypsosimulationmodel.core.Assert.4", paramName ); //$NON-NLS-1$
      throw new IllegalArgumentException( message );
    }

    if( param.isEmpty() )
    {
      final String message =  Messages.getString("org.kalypso.kalypsosimulationmodel.core.Assert.5", paramName ); //$NON-NLS-1$
      throw new IllegalArgumentException( message );
    }

  }

  public static final String throwIAEOnNullOrEmpty( String str ) throws IllegalArgumentException
  {
    if( str == null )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.9" ) ); //$NON-NLS-1$
    }
    str = str.trim();
    if( str.length() == 0 )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.10" ) ); //$NON-NLS-1$
    }
    return str;
  }

  public static final boolean isNullOrEmpty( String str )
  {
    if( str == null )
    {
      return true;
    }
    str = str.trim();
    if( str.length() == 0 )
    {
      return true;
    }
    return false;
  }

  public static final void throwIAEOnLessThan0( final double d, String message ) throws IllegalArgumentException
  {
    if( d < 0 )
    {
      if( message == null )
      {
        message = Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.11" ); //$NON-NLS-1$
      }
      throw new IllegalArgumentException( message );
    }
  }

  public static final void throwIAEOnNotDirectInstanceOf( final Feature feature, final QName expectedType ) throws IllegalArgumentException
  {
    if( !Util.directInstanceOf( feature, expectedType ) )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.12" ) + //$NON-NLS-1$
          expectedType + Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.13" ) + feature.getFeatureType().getQName() ); //$NON-NLS-1$
    }
  }

  public static final void throwIAEOnNulOrIsDirOrNotExistsOrNotReadable( final File file ) throws IllegalArgumentException
  {
    if( file == null )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.14" ) ); //$NON-NLS-1$
    }

    if( file.isDirectory() )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.15" ) + file ); //$NON-NLS-1$
    }

    if( !file.exists() )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.16" ) + file ); //$NON-NLS-1$
    }

    if( !file.canRead() )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.Assert.17" ) + file ); //$NON-NLS-1$
    }

  }
}
