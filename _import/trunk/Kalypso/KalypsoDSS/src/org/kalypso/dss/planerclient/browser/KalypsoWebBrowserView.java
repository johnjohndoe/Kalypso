package org.kalypso.dss.planerclient.browser;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.LocationAdapter;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.internal.browser.BrowserViewer;
import org.eclipse.ui.internal.browser.IBrowserViewerContainer;
import org.eclipse.ui.part.ViewPart;

public class KalypsoWebBrowserView extends ViewPart implements
		IBrowserViewerContainer, IKalypsoLinkHandler {

	public static final String WEB_BROWSER_VIEW_ID = "org.kalypso.dss.planerclient.view.browser"; //$NON-NLS-1$

	private IMemento m_memento;

	// Persistance tags.

	private static final String TAG_URL = "url"; //$NON-NLS-1$

	private static final String TAG_SCROLLBARS = "scrolbars";

	private static final String TAG_HORIZONTAL_BAR = "horizontal";

	private static final String TAG_VERTICAL_BAR = "vertical";

	private static final String TAG_SELECTION = "selection";

	protected BrowserViewer viewer;

	private LocationListener locationListener = new LocationAdapter() {
		@Override
    public void changed(LocationEvent event) {
			event.doit = false;
			String link = event.location;
			if (link.startsWith(IKalypsoLinkHandler.PERSPECTIV_PROTOCOL)) {
				String linkStr = link.replaceFirst(
						IKalypsoLinkHandler.PERSPECTIV_PROTOCOL, "");
				handlePerspectiveProtocol(linkStr);

			} else if (link.startsWith(IKalypsoLinkHandler.LAUNCH_PROTOCOL)) {
				MessageDialog.openConfirm(getSite().getShell(),
						"Not implemented yet",
						"Das launch-protokol ist noch nicht implementiert.");
			}
		}
	};

	public KalypsoWebBrowserView() {
	}

	@Override
  public void init(IViewSite site, IMemento memento) throws PartInitException {
		super.init(site, memento);
		m_memento = memento;
	}

	private void addBrowserListener() {
		viewer.getBrowser().addLocationListener(locationListener);
	}

	private void removeLocationListener() {
		if (viewer.getBrowser().isDisposed())
			return;
		viewer.getBrowser().removeLocationListener(locationListener);
	}

	@Override
  public void createPartControl(Composite parent) {
		viewer = new BrowserViewer(parent, SWT.NONE);// BrowserViewer.LOCATION_BAR
		viewer.setContainer(this);
		addBrowserListener();
		if (m_memento != null)
			restoreState(m_memento);
		m_memento = null;
	}

	protected void restoreState(IMemento memento) {
		String url = memento.getString(TAG_URL);
		// set the url of the browser
		viewer.setURL(url);
		if (viewer.combo != null)
			viewer.combo.setText(url);

		IMemento scrollbars = memento.getChild(TAG_SCROLLBARS);
		if (scrollbars == null)
			return;
		IMemento horizontal = scrollbars.getChild(TAG_HORIZONTAL_BAR);
		if (horizontal != null) {
			int hSelection = horizontal.getInteger(TAG_SELECTION).intValue();
			ScrollBar horizontalBar = viewer.getHorizontalBar();
			if (horizontalBar != null)
				horizontalBar.setSelection(hSelection);
		}
		IMemento vertical = scrollbars.getChild(TAG_VERTICAL_BAR);
		if (vertical != null) {
			int vSelection = vertical.getInteger(TAG_SELECTION).intValue();
			ScrollBar verticalBar = viewer.getVerticalBar();
			if (verticalBar != null)
				verticalBar.setSelection(vSelection);
		}
	}

	@Override
  public void dispose() {
		removeLocationListener();
		if (viewer != null)
			viewer.dispose();
		super.dispose();
	}

	public void setURL(String url) {
		if (viewer != null)
			viewer.setURL(url);
	}

	@Override
  public void setFocus() {
		viewer.setFocus();
	}

	public boolean close() {
		try {
			getSite().getPage().hideView(this);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	public IActionBars getActionBars() {
		return getViewSite().getActionBars();
	}

	public void openInExternalBrowser(String url) {
		try {
			URL theURL = new URL(url);
			IWorkbenchBrowserSupport support = PlatformUI.getWorkbench()
					.getBrowserSupport();
			support.getExternalBrowser().openURL(theURL);
		} catch (MalformedURLException e) {
			// TODO handle this
		} catch (PartInitException e) {
			// TODO handle this
		}
	}

	/**
	 * Return true if the filename has a "web" extension.
	 * 
	 * @param name
	 * @return
	 */
	protected boolean isWebFile(String name) {
		return name.endsWith("html") || name.endsWith("htm") || name.endsWith("gif") || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				name.endsWith("jpg"); //$NON-NLS-1$
	}

	public boolean handleLaunchProtocol(Object link) {
		return false;
	}

	@Override
  public void saveState(IMemento memento) {
		if (viewer == null) {
			if (m_memento != null) // Keep the old state;
				memento.putMemento(m_memento);
			return;
		}
		memento.putString(TAG_URL, viewer.getURL());
		IMemento scrollbarMemento = memento.createChild(TAG_SCROLLBARS);
		ScrollBar horizontalBar = viewer.getHorizontalBar();
		if (horizontalBar != null) {
			IMemento horizontal = scrollbarMemento
					.createChild(TAG_HORIZONTAL_BAR);
			horizontal.putInteger(TAG_SELECTION, horizontalBar.getSelection());
		}
		ScrollBar verticalBar = viewer.getVerticalBar();
		if (verticalBar != null) {
			IMemento vertical = scrollbarMemento.createChild(TAG_VERTICAL_BAR);
			vertical.putInteger(TAG_SELECTION, verticalBar.getSelection());
		}
	}

	public boolean handlePerspectiveProtocol(String url) {
		String[] split = url.split(IKalypsoLinkHandler.SEPERATOR);
		// try {
		// KalypsoPerspectiveHelper.nextPerspective(split[0], split[1]);
		// } catch (WorkbenchException e) {
		// e.printStackTrace();
		// return false;
		// } catch (FileNotFoundException e) {
		// e.printStackTrace();
		// return false;
		// }
		// return true;
		return KalypsoPerspectiveHelper.nextPerspective(split[0], split[1],
				getSite().getPage());
	}

}
