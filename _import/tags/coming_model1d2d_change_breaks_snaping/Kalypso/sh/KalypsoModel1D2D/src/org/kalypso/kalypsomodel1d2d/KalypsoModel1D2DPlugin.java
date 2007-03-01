/**
 * 
 */
package org.kalypso.kalypsomodel1d2d;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * @author Gernot Belger
 */
public class KalypsoModel1D2DPlugin extends AbstractUIPlugin {

	// The shared instance.
	private static KalypsoModel1D2DPlugin plugin;

	public KalypsoModel1D2DPlugin() {
		plugin = this;
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared instance.
	 */
	public static KalypsoModel1D2DPlugin getDefault() {
		return plugin;
	}

}
