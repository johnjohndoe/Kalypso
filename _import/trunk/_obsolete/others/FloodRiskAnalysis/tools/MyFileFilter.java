package tools;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class MyFileFilter extends FileFilter
{
  String[] m_extensions = null;

  public MyFileFilter( String[] extensions )
  {
    super();
    m_extensions = extensions;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
   */
  public boolean accept( File f )
  {
    if( f.isDirectory() )
    {
      return true;
    }
    boolean returnValue = false;

    String extension = getExtension( f );
    if( extension != null )
    {
      for( int i = 0; i < m_extensions.length; i++ )
      {
        if( extension.equals( m_extensions[i] ) )
        {
          returnValue = true;
        }
        else
        {
          returnValue = false;
        }
      }
    }

    return returnValue;
  }

  /*
   * Get the extension of a file.
   */
  private String getExtension( File f )
  {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( i + 1 ).toLowerCase();
    }
    return ext;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription()
  {
    return null;
  }

}