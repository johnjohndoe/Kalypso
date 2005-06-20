package org.kalypso.metadoc;

import java.io.File;
import java.io.IOException;
import java.util.Map;

/**
 * Document to be used within the MetaDoc-Framework. It wraps a temporary file that can be used by clients to throw
 * content into it. Once the document is ready, it is likely to be send over the metadoc service for publication.
 * 
 * @author schlienger
 */
public class Document
{
  private final String m_fileExtension;

  private final Map m_metadata;

  private final File m_file;

  public Document( final String fileExtension, final Map metadata ) throws IOException
  {
    m_fileExtension = fileExtension;
    m_metadata = metadata;

    m_file = File.createTempFile( "document", fileExtension );
    m_file.deleteOnExit();
  }

  public void dispose()
  {
    m_metadata.clear();
    m_file.delete();
  }

  public Map getMetadata()
  {
    return m_metadata;
  }

  public String getFileExtension()
  {
    return m_fileExtension;
  }

  public File getFile()
  {
    return m_file;
  }
}