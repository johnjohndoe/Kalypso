package org.kalypso.util.transformation;

import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.model.xml.TransformationList;
import org.kalypso.model.xml.TransformationType;

/**
 * Static Helper class for transformations
 * 
 * @author belger
 */
public class TransformationHelper
{
  /** Creates a 'real' {@link ITransformation} from a {@link TransformationType XML-TransformationType}. */
  public static ITransformation createTransformation( final TransformationType trans ) throws TransformationException
  {
    try
    {
      final ITransformation transformation = (ITransformation)ClassUtilities.newInstance( trans.getClassName(), ITransformation.class, TransformationHelper.class.getClassLoader() );
      
      final Properties props = new Properties();
      final List arguments = trans.getArgument();
      for( Iterator argIter = arguments.iterator(); argIter.hasNext(); )
      {
        final TransformationType.ArgumentType argument = (TransformationType.ArgumentType)argIter.next();
        props.setProperty( argument.getName(), argument.getValue() );
      }
      
      transformation.setProperties( props );
      
      return transformation;
    }
    catch( final ClassUtilityException e )
    {
      throw new TransformationException( "Could not create Transformation: " + trans.getClassName(), e );
    }
  }

  /** Führt alle Transformationen einer Transformationen Liste aus */
  public static void doTranformations( final TransformationList trans,
      final IProgressMonitor monitor ) throws TransformationException
  {
    final List transList = trans.getTransformation();

    monitor.beginTask( "Transformationen durchführen", transList.size() );

    for( final Iterator iter = transList.iterator(); iter.hasNext(); )
    {
      final TransformationType element = (TransformationType)iter.next();
      final ITransformation ccTrans = TransformationHelper
          .createTransformation( element );
      
      ccTrans.transform( new SubProgressMonitor( monitor, 1 ) );
      
      monitor.worked( 1 );
    }
    
    monitor.done();
  }

}
