from qiskit import QuantumCircuit, execute
from qiskit.providers.aer import AerSimulator
from qiskit.visualization import plot_histogram
from qiskit import IBMQ as IBMQ

qc = QuantumCircuit(8, 3)

qc.x(0)
qc.x(1)
qc.x(2)
qc.x(3)

qc.cx(1, 7)
qc.cx(3, 7)

qc.ccx(3, 1, 4)

qc.cx(0, 6)
qc.cx(2, 6)
qc.cx(4, 6)

qc.ccx(2, 0, 5)
qc.ccx(4, 2, 5)
qc.ccx(4, 0, 5)




qc.measure([5, 6, 7], [2, 1, 0])

"""sim = AerSimulator()

job = sim.run(qc)

result = job.result()
print(qc.draw())
print(result.get_counts())"""

provider = IBMQ.load_account()

ourense = provider.get_backend('ibmq_qasm_simulator')

job = execute(qc, ourense)

job.wait_for_final_state()

result = job.result()
counts = result.get_counts()
plot_histogram(counts)