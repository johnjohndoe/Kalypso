package org.kalypso.model.km.internal.core;

import java.io.File;

import org.kalypso.model.km.internal.i18n.Messages;

// FIXME: cannot work because of the eclipse dependencies. If ever, we need to make an IApplication from this code.
public class Main
{
  public static void main( final String[] args )
  {
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.0" ) ); //$NON-NLS-1$
    System.out.println();
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.1" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.2" ) ); //$NON-NLS-1$
    System.out.println();
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.3" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.4" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.5" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.6" ) ); //$NON-NLS-1$
    System.out.println();
    System.out.println( Messages.getString( "org.kalypso.model.km.Main.7" ) ); //$NON-NLS-1$
    System.out.println();
    try
    {
      if( args.length != 4 )
        throw new Exception( Messages.getString( "org.kalypso.model.km.Main.8" ) ); //$NON-NLS-1$
      System.out.println( Messages.getString( "org.kalypso.model.km.Main.9" ) ); //$NON-NLS-1$
      final double kmStart = Double.parseDouble( args[0] );
      System.out.println( "kmStart: " + kmStart + " [km]" ); //$NON-NLS-1$ //$NON-NLS-2$

      final double kmEnd = Double.parseDouble( args[1] );
      System.out.println( "kmEnd:   " + kmEnd + " [km]" ); //$NON-NLS-1$ //$NON-NLS-2$
      final int nkm = Integer.parseInt( args[2] );
      System.out.println( "nkm:     " + nkm ); //$NON-NLS-1$

      final File path = new File( args[3] );
      System.out.println( "path:    " + path.getAbsolutePath() ); //$NON-NLS-1$
      if( !path.exists() )
        throw new Exception( Messages.getString( "org.kalypso.model.km.Main.16" ) + path.toString() ); //$NON-NLS-1$
      System.out.println();
      final ProfileDataSet pSet = ProfileFactory.createProfileSet( path, kmStart, kmEnd );
      final IKMValue[] kmValues = pSet.getKMValues();
      System.out.println( Messages.getString( "org.kalypso.model.km.Main.17" ) ); //$NON-NLS-1$
      for( final IKMValue value : kmValues )
      {
        System.out.println( value );
      }
    }
    catch( final Exception e )
    {
      System.out.println( Messages.getString( "org.kalypso.model.km.Main.18" ) ); //$NON-NLS-1$
      System.out.println( Messages.getString( "org.kalypso.model.km.Main.19" ) + e.getMessage() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      System.out.println( Messages.getString( "org.kalypso.model.km.Main.21" ) + e.getLocalizedMessage() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
