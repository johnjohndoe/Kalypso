package org.kalypso.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Properties;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.LoaderException;

/**
 * @author schlienger
 *  
 */
public class SLDLoader implements ILoader
{
  public SLDLoader()
  {
  // nothing
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "OGC SLD";
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#load(java.util.Properties,
   *      java.lang.Object)
   */
  public Object load( final Properties source, final Object helper ) throws LoaderException
  {
    try
    {
      final IProject project = (IProject)helper;

      final String sourcePath = source.getProperty( "PATH", "" );
      final IFile file = project.getFile( sourcePath );

      final Reader reader = new InputStreamReader( file.getContents() );
      final StyledLayerDescriptor myStyledLayerDescriptor = SLDFactory.createSLD( reader );
      reader.close();

      return myStyledLayerDescriptor;
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
   */
  public void save( final Properties source, final Object data ) throws LoaderException
  {
    // TODO: Transformation vom laden muss rueckgaengig gemacht werden !
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#setSource(java.util.Properties)
   */
  public void setSource( final Properties source )
  {
  //
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getSource()
   */
  public Properties getSource()
  {
    return null;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#createControl(java.lang.Object)
   */
  public Object createControl( final Object argument )
  {
    final Composite c = new Composite( (Composite)argument, SWT.NONE );
    return c;
  }
}