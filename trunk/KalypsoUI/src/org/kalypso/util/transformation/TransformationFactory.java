package org.kalypso.util.transformation;

import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.model.xml.TransformationType;

/**
 * Static Helper class for transformations
 * 
 * @author belger
 */
public class TransformationFactory
{
  public static ITransformation createTransformation( final TransformationType trans ) throws TransformationException
  {
    try
    {
      final ITransformation calcTrans = (ITransformation)ClassUtilities.newInstance( trans.getClassName(), ITransformation.class, TransformationFactory.class.getClassLoader() );
      
      final Properties props = new Properties();
      final List args = trans.getArg();
      for( Iterator argIter = args.iterator(); argIter.hasNext(); )
      {
        final TransformationType.ArgType arg = (TransformationType.ArgType)argIter.next();
        props.setProperty( arg.getName(), arg.getValue() );
      }
      
      calcTrans.setProperties( props );
      
      return calcTrans;
    }
    catch( final ClassUtilityException e )
    {
      throw new TransformationException( "Could not create Transformation: " + trans.getClassName(), e );
    }
  }
}
