/**
 * 
 */
package org.kalypso.kalypsomodel1d2d;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.osgi.framework.BundleContext;

/**
 * @author Gernot Belger
 */
public class KalypsoModel1D2DPlugin extends AbstractUIPlugin
{

  // The shared instance.
  private static KalypsoModel1D2DPlugin plugin;
  
  private ResultDB resultDB;

  public KalypsoModel1D2DPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
    try
    {
      resultDB = new ResultDB(ResultDB.getFolder());
    }catch (Exception e) {
      e.printStackTrace();
      resultDB = null;
    }
  }

    
  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    try
    {
      resultDB.save();
    }
    catch (Exception e) 
    {
      e.printStackTrace();
    }
    
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoModel1D2DPlugin getDefault( )
  {
    return plugin;
  }
  
  public ResultDB getResultDB()
  {
    return resultDB;
  }
}
