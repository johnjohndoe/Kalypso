package org.kalypso.ui.metadoc.util;

import java.io.OutputStream;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.metadoc.IExportableDocument;

/**
 * @author belger
 */
public class MultiDocumentServiceWrapper
{
  private final IExportableDocument m_dummyDoc = new IExportableDocument()
  {
    public void exportDocument( final OutputStream outs ) throws Exception
    {
    // ignore
    }

    public String getDocumentExtension()
    {
      return ".dummy";
    }
  };
  
  private MetadocServiceWrapper m_wrapper;

  
  public MultiDocumentServiceWrapper() throws CoreException
  {
    final String username = System.getProperty( "user.name" );

    m_wrapper = new MetadocServiceWrapper( m_dummyDoc.getDocumentExtension(), username );
  }


  public IExportableDocument getDummyDoc()
  {
    return m_dummyDoc;
  }


  public DocBean getDoc()
  {
    return m_wrapper.getDoc();
  }


  public void dispose()
  {
    try
    {
      m_wrapper.cancelData();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }
  
}
