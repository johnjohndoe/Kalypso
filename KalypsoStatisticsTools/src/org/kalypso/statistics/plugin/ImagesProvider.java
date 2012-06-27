package org.kalypso.statistics.plugin;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.kalypso.statistics.types.EStatisticsImage;

public class ImagesProvider {

	public static final Image getImage(final EStatisticsImage image) {
		final ImageRegistry imageRegistry = KalypsoStatisticsPlugin.getDefault().getImageRegistry();
		return imageRegistry.get(image.name());
	}

	public static final ImageDescriptor getImageDescriptor(final EStatisticsImage image) {
		final ImageRegistry imageRegistry = KalypsoStatisticsPlugin.getDefault().getImageRegistry();
		return imageRegistry.getDescriptor(image.name());
	}
}
