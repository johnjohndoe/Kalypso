package org.kalypso.psiadapter;

import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.psiadapter.util.AtWQProvider;
import org.kalypso.psiadapter.util.DummyWQProvider;
import org.kalypso.psiadapter.util.IWQProvider;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompactImpl;

/**
 * The entry point to the PSICompact interface from PSI.
 * 
 * @author schlienger
 */
public final class PSICompactFactory
{

  /**
   * Name of the system property which contains the location of the fake data. Must be an url. Normaly this points to
   * the directory containing the 'structure.txt' file. <br/>ATTENTION: If this property is set, the fake implementation
   * is used instead of the real PSICompact implementation!
   */
  private static final String SYSPROP_FAKE_LOCATION = "kalypso.psifake.location";

  protected static PSICompact m_psiCompact = null;

  /**
   * Name of the system property which contains the location of the dictionary of .at files. <br>
   * The dictionary is in the properties-file format (@link{Properties}), each key representing a PSI-Object-ID, each
   * value representing the (relative) path to an .at file. <br>
   */
  public static final String SYSPROP_AT_DICTIONARY = "kalypso.psi.at.properties";

  private static IWQProvider s_wqProvider = null;

  /**
   * Returns the connection to the PSI-Interface implementation.
   * 
   * @return PSICompact instance
   */
  public final static PSICompact getConnection( )
  {
    if( m_psiCompact != null )
    {
      try
      {
        // fake Aufruf nur um zu testen ob die SST da ist
        m_psiCompact.getDataModelVersion();
      }
      catch( final ECommException e )
      {
        m_psiCompact = null; // damit wird es neu initialisiert

        // es geht im nächsten if-Block weiter
        // damit wird es neu initialisiert (vielleicht wurde psicompact inzwischen neu gestartet)
      }
    }

    if( m_psiCompact == null )
    {
      try
      {
        final String fakeLocation = FrameworkProperties.getProperty( SYSPROP_FAKE_LOCATION, null );
        if( fakeLocation == null )
        {
          System.out.println( "Fake location not set, using real PSICompact-implementation. Use the following System-property to use a fake implementation instead: " + SYSPROP_FAKE_LOCATION );
          m_psiCompact = new PSICompactImpl();
        }
        else
        {
          System.out.println( "Fake location set. Using PSI-fake implementation on location: " + fakeLocation );
          m_psiCompact = new PSICompactFakeImpl( fakeLocation );
        }

        // Wichtig! init() aufrufen damit die PSI-Schnittstelle sich
        // initialisieren kann
        m_psiCompact.init();
      }
      catch( final Exception e )
      {
        // TODO: why not set m_psiCompact to null?
        e.printStackTrace();

        throw new IllegalStateException( "Error while creating PSICompact: " + e.toString() );
      }
    }

    return m_psiCompact;
  }

  public static IWQProvider getWQProvider( )
  {
    if( s_wqProvider == null )
    {
      try
      {
        final String atDictLocation = FrameworkProperties.getProperty( SYSPROP_AT_DICTIONARY, null );
        if( atDictLocation != null )
          s_wqProvider = new AtWQProvider( atDictLocation );
      }
      catch( final Throwable e )
      {
        System.out.println( "Failed to load AT-Dictionary: " + e.getLocalizedMessage() );
        e.printStackTrace();
      }
      finally
      {
        // Set dummy provider,so we do not try to load again
        if( s_wqProvider == null )
          s_wqProvider = new DummyWQProvider();
      }
    }

    return s_wqProvider;
  }
}