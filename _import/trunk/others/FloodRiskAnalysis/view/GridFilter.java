package view;

import java.io.File;
import java.io.FileFilter;

/**
 * FileFilter, which accepts just AsciiGrid-Files (.asc)
 * 
 * @author N. Peiler
 *  
 */
public class GridFilter implements FileFilter
{

  //Accept all asc files.
  public boolean accept( File f )
  {

    String extension = null;
    String s = f.getName();
    int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      extension = s.substring( i + 1 ).toLowerCase();
    }
    boolean flag = false;
    if( extension != null )
    {
      if( extension.equals( "asc" ) )
      {
        flag = true;
      }
      else
      {
        flag = false;
      }
    }
    return flag;
  }

  //The description of this filter
  public String getDescription()
  {
    return "Just Ascii-Grids";
  }
}