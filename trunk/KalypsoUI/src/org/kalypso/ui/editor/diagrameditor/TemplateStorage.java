package org.kalypso.ui.editor.diagrameditor;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TemplateStorage
 * 
 * @author schlienger
 */
public class TemplateStorage implements IEncodedStorage
{
  private LinkedDiagramTemplate m_template;

  private IPath m_path;

  public TemplateStorage( final LinkedDiagramTemplate template,
      final IPath path )
  {
    m_template = template;
    m_path = path;
  }

  /**
   * @return Returns the template.
   */
  public LinkedDiagramTemplate getTemplate( )
  {
    return m_template;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getContents()
   */
  public InputStream getContents( ) throws CoreException
  {
    try
    {
      final ObsdiagviewType tType = ObservationTemplateHelper
          .buildDiagramTemplateXML( m_template );

      final StringWriter writer = new StringWriter();
      
      // writer is closed during call
      ObservationTemplateHelper.saveDiagramTemplateXML( tType, writer );

      final byte[] bytes = writer.toString().getBytes( "UTF-8" );

      return new ByteArrayInputStream( bytes );
    }
    catch( Exception e ) // generic for simplicity
    {
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "", e ) );
    }
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getFullPath()
   */
  public IPath getFullPath( )
  {
    return m_path;
  }

  /**
   * @see org.eclipse.core.resources.IStorage#getName()
   */
  public String getName( )
  {
    return m_template.getTitle();
  }

  /**
   * @see org.eclipse.core.resources.IStorage#isReadOnly()
   */
  public boolean isReadOnly( )
  {
    return false;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    return null;
  }

  /**
   * @see org.eclipse.core.resources.IEncodedStorage#getCharset()
   */
  public String getCharset( ) throws CoreException
  {
    return "UTF-8";
  }
}