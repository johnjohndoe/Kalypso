package de.renew.workflow;

import java.io.IOException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;
import java.util.Vector;

import de.renew.access.LoginInfo;
import de.renew.access.Manager;
import de.renew.remote.RemoteServerRegistry;
import de.renew.remote.SocketFactoryDeterminer;
import de.renew.remote.RemoteServerRegistry.ServerDescriptor;
import de.renew.util.ConcurrencyPreventer;
import de.renew.workflow.event.IWorklistChangeListener;

public class WorkflowConnector {
	private static final String SERVICE_NAME = "de.renew.workflow.WorkflowManager";

	private LoginInfo m_login;

	private List<IWorklistChangeListener> m_listenerCache;

	private AgendaChangeListener m_listenerProxy;

	private Manager m_manager;

	private Activity m_activity;

	private static WorkflowConnector _connector;

	private boolean _connected = false;

	/**
	 * @return the WorkflowConnector if connection is established, null
	 *         otherwise
	 */
	public static WorkflowConnector getConnector() {
		if (_connector == null) {
			_connector = new WorkflowConnector();
		}
		if (!_connector.isConnected()) {
			_connector.connect();
		}
		return _connector;
	}

	public boolean isConnected() {
		return _connected;
	}

	private WorkflowConnector() {
	}

	/**
	 * Starts a login-dialog and - if the login is successful - the AgendaGUI.
	 * 
	 * @return true, if the GUI properly started. false, if an error occured
	 *         (e.g. the login was not successful)
	 */
	protected void connect() {
		boolean success = false;
		try {
			final ServerDescriptor descriptor = RemoteServerRegistry.instance()
					.connectServer("localhost", "default");
			m_manager = (Manager) Naming
					.lookup(descriptor.getUrl(SERVICE_NAME));
			m_login = new LoginInfo("Stefan", "Stefan");
			m_manager.checkLogin(m_login);
			m_listenerProxy = new AgendaChangeListenerProxy();
			getAgenda().addChangeListener(m_listenerProxy);
			m_listenerProxy.notifyAgendaChange(new AgendaChangeEvent(this,
					AgendaChangeEvent.AVAILABLES_CHANGED));
			m_listenerProxy.notifyAgendaChange(new AgendaChangeEvent(this,
					AgendaChangeEvent.REQUESTEDS_CHANGED));
			success = true;
		} catch (final RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NotBoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		_connected = success;
	}

	private Agenda getAgenda() throws RemoteException, IOException {
		if (m_login != null) {
			return ((WorkflowManager) m_manager).getAgenda(m_login);
		} else {
			return null;
		}
	}

	public WorkItem[] getAvailables() {
		if (m_login != null) {
			try {
				return getAgenda().getAvailables();
			} catch (final RemoteException e) {
				// TODO: log
				e.printStackTrace();
			} catch (final IOException e) {
				// TODO: log
				e.printStackTrace();
			}
		}
		return new WorkItem[0];
	}

	public Activity[] getRequesteds() {
		if (m_login != null) {
			try {
				return getAgenda().getRequesteds();
			} catch (final RemoteException e) {
				// TODO: log
				e.printStackTrace();
			} catch (final IOException e) {
				// TODO: log
				e.printStackTrace();
			}
		}
		return new Activity[0];
	}

	public class AgendaChangeListenerProxy extends UnicastRemoteObject
			implements AgendaChangeListener {

		private static final long serialVersionUID = -4720319067463967570L;

		/**
		 * The concurrent update preventer for the availables.
		 */
		private final ConcurrencyPreventer availablesConcurrencyPreventer;

		/**
		 * Creates a new agenda change listener.
		 * 
		 * @exception RemoteException
		 *                An RMI problem occurred.
		 */
		private AgendaChangeListenerProxy() throws RemoteException {
			super(0, SocketFactoryDeterminer.getInstance(),
					SocketFactoryDeterminer.getInstance());
			availablesConcurrencyPreventer = new ConcurrencyPreventer(
					new AvailablesUpdater());
		}

		/**
		 * Notifies the listener about a change in an agenda.
		 * 
		 * @param event
		 *            The agenda change event.
		 * @exception RemoteException
		 *                An RMI problem occurred.
		 */
		public void notifyAgendaChange(final AgendaChangeEvent event)
				throws RemoteException {
			availablesConcurrencyPreventer.requestRun();
		}

		/**
		 * The runnable for the concurrency preventer to update the availables.
		 */
		private class AvailablesUpdater implements Runnable {

			public void run() {
				// Display.getDefault().asyncExec(new Runnable() {
				// public void run() {
				// try {
				for (final IWorklistChangeListener listener : m_listenerCache) {
					listener.worklistChanged();
				}
				// } catch (final Exception e) {
				// e.printStackTrace();
				// // TODO: logger.error(e.getMessage(), e);
				// }
				// }
				// });
			}
		}
	}

	protected void disconnect() {
		if (m_login != null) {
			try {
				getAgenda().removeChangeListener(m_listenerProxy);
			} catch (final RemoteException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} finally {
				m_listenerCache.clear();
				m_login = null;
			}
		}
	}

	public void addWorklistChangeListener(
			final IWorklistChangeListener worklistChangeListener) {
		if (m_listenerCache == null) {
			m_listenerCache = new Vector<IWorklistChangeListener>();
		}
		m_listenerCache.add(worklistChangeListener);
	}

	public void removeWorklistChangeListener(
			final IWorklistChangeListener worklistChangeListener) {
		m_listenerCache.remove(worklistChangeListener);
	}

	/**
	 * Requests a new WorkItem and confirms the active Activity, if there is one
	 */
	public void request(final WorkItem workItem) {
		try {
			if (m_activity != null) {
				confirm(m_activity);
			}
			m_activity = workItem.request(m_login, new ClientImpl(m_login));
		} catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void confirm(final Activity activity) throws RemoteException,
			SecurityException {
		activity.confirm(m_login, new ClientImpl(m_login), null);
	}
}
