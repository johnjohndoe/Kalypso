package org.kalypso.util.serializer;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;

/**
 * ISerializer denotes a class that can be serialized using read and write. The interface is pretty much straight
 * forward and allows an object to be written to and read from a stream.
 * 
 * @author schlienger
 */
public interface ISerializer
{
  /**
   * Reads an object from the input stream
   * 
   * @param ins
   *          InputStream to read from
   * @return new instance of object read from stream
   */
  public Object read( final InputStream ins ) throws InvocationTargetException;

  /**
   * Writes the given object to the output stream
   * 
   * @param object
   *          object to write
   * @param os
   *          OutputStream to write to
   */
  public void write( final Object object, final OutputStream os ) throws InvocationTargetException;
}
