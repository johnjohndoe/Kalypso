package org.kalypso.ui.metadoc;

import java.io.OutputStream;

/**
 * IExportableDocument
 * 
 * @author schlienger
 */
public interface IExportableDocument
{
  /**
   * Exports the document using the writer.
   * 
   * @param outs
   * @throws Exception
   */
  public void exportDocument( final OutputStream outs ) throws Exception;
  
  /**
   * @return the extension of the document file (must include the point as in '.txt')
   */
  public String getDocumentExtension();
}
