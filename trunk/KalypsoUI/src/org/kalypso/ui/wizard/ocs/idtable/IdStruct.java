package org.kalypso.ui.wizard.ocs.idtable;

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.ocs.repository.ServiceRepositoryObservation;

/**
 * Structure containing an observation file and its server side Identifier.
 * 
 * @author schlienger
 */
public class IdStruct
{
  private final IFile m_file;
  private final String m_id;

  public IdStruct( final IFile file, final String id )
  {
    m_file = file;
    m_id = id;
  }
  
  /**
   * @return Returns the file.
   */
  public IFile getFile( )
  {
    return m_file;
  }
  
  /**
   * @return Returns the id.
   */
  public String getId( )
  {
    return m_id;
  }
  
  /**
   * Creates using the metadata found in the file
   * 
   * @param file
   * @return id container
   */
  public static IdStruct createUsing( final IFile file )
  {
    if( FileUtilities.getSuffix( file.getName() ).equalsIgnoreCase( "zml" ) )
    {
      try
      {
        final URL url = ResourceUtilities.createURL( file );
        final IObservation obs = ZmlFactory.parseXML( url, url.toString() );
        
        final String id = obs.getMetadataList().getProperty( ServiceRepositoryObservation.MD_OCS_ID, "<kein KZ>" );

        return new IdStruct( file, id );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    
    return null;
  }
}
