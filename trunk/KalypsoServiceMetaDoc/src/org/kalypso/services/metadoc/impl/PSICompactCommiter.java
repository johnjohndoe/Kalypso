package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.psiadapter.metadoc.MetaDocSerializer;
import org.kalypso.services.metadoc.DocBean;
import org.kalypso.services.metadoc.IMetaDocCommiter;
import org.kalypso.services.metadoc.MetaDocException;

/**
 * PSICompactCommiter
 * 
 * @author schlienger
 */
public class PSICompactCommiter implements IMetaDocCommiter
{
  /**
   * @see org.kalypso.services.metadoc.IMetaDocCommiter#prepareMetainf(org.kalypso.services.metadoc.DocBean)
   */
  public void prepareMetainf( final DocBean docBean )
  {
    final Properties props = new Properties();
    props.putAll( docBean.getMetadata() );

    MetaDocSerializer.prepareProperties( props );

    docBean.getMetadata().putAll( props );
  }

  /**
   * @see org.kalypso.services.metadoc.IMetaDocCommiter#commitDocument(org.kalypso.services.metadoc.DocBean)
   */
  public void commitDocument( final DocBean docBean ) throws MetaDocException
  {
    final File xmlFile = new File( FileUtilities.nameWithoutExtension( docBean
        .getLocation() )
        + ".xml" );

    FileWriter writer = null;

    try
    {
      writer = new FileWriter( xmlFile );

      final Properties props = new Properties();
      props.putAll( docBean.getMetadata() );

      MetaDocSerializer.buildXML( props, writer, docBean.getLocation() );
    }
    catch( IOException e )
    {
      throw new MetaDocException( e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}