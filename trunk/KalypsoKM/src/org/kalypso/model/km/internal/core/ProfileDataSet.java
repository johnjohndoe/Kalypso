package org.kalypso.model.km.internal.core;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileDataSet extends AbstractProfileDataSet
{
  /**
   * The profile files.
   */
  private File[] m_profileFiles;

  /**
   * The constructor.
   * 
   * @param profileFiles
   *          The profile files.
   */
  public ProfileDataSet( File[] profileFiles )
  {
    super( Double.NaN, Double.NaN );

    m_profileFiles = profileFiles;
  }

  /**
   * The constructor.
   * 
   * @param profileFiles
   *          The profile files.
   * @param startPosition
   *          The start position (the first station). In meters!
   * @param endPosition
   *          The end position (the last station). In meters!
   */
  public ProfileDataSet( File[] profileFiles, double startPosition, double endPosition )
  {
    super( startPosition, endPosition );

    m_profileFiles = profileFiles;
  }

  /**
   * @see org.kalypso.model.km.internal.core.AbstractProfileDataSet#createProfileData()
   */
  @Override
  protected void createProfileData( )
  {
    for( File file : m_profileFiles )
    {
      try
      {
        ProfileData qwProfile = ProfileFactory.createQWProfile( file, getStartPosition(), getEndPosition() );

        addProfileData( qwProfile );
      }
      catch( IOException e )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString( "org.kalypso.model.km.ProfileDataSet.0" ) + file.getAbsolutePath() ); //$NON-NLS-1$
        e.printStackTrace();
      }
    }
  }
}